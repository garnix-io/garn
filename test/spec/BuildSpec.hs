{-# LANGUAGE QuasiQuotes #-}

module BuildSpec where

import Data.String.Interpolate (i)
import Development.Shake (StdoutTrim (..), cmd)
import System.Directory
import System.Exit (ExitCode (..))
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils

spec :: Spec
spec = do
  describe "build" $ do
    repoDir <- runIO getCurrentDirectory
    around_
      ( withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]
          . inTempDirectory
      )
      . around onTestFailureLogger
      $ do
        it "builds packages and creates a result link" $ \onTestFailureLog -> do
          writeHaskellProject repoDir
          output <- runGarn ["build", "foo"] "" repoDir Nothing
          onTestFailureLog output
          doesDirectoryExist "result" `shouldReturn` True
          StdoutTrim output <- cmd ("result/bin/garn-test" :: String)
          output `shouldBe` ("haskell test output" :: String)

        it "complains about packages that cannot be built" $ \onTestFailureLog -> do
          writeHaskellProject repoDir
          writeFile
            "Main.hs"
            [i|
              main :: IO ()
              main = "foo"
            |]
          output <- runGarn ["build", "foo"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldContain` "Couldn't match type"
          exitCode output `shouldBe` ExitFailure 1

        describe ".build" $ do
          it "builds manually specified packages" $ \onTestFailureLog -> do
            writeFile
              "garn.ts"
              [i|
                import * as garn from "#{repoDir}/ts/mod.ts"

                export const project = garn.mkProject(
                  { description: "" },
                  {
                    package: garn.build`
                      echo "build-content" > $out/build-artifact
                    `,
                  },
                )
              |]
            output <- runGarn ["build", "project"] "" repoDir Nothing
            onTestFailureLog output
            readFile "result/build-artifact" `shouldReturn` "build-content\n"
            exitCode output `shouldBe` ExitSuccess

          it "makes tools from the environment available" $ \onTestFailureLog -> do
            writeFile
              "garn.ts"
              [i|
                import * as garn from "#{repoDir}/ts/mod.ts"
                import * as nix from "#{repoDir}/ts/nix.ts"

                export const project = garn.mkProject(
                  { description: "" },
                  {
                    package:
                      garn
                        .emptyEnvironment
                        .withDevTools([garn.mkPackage(nix.nixRaw`pkgs.hello`)])
                        .build`
                          hello > $out/build-artifact
                        `,
                  },
                )
              |]
            output <- runGarn ["build", "project"] "" repoDir Nothing
            onTestFailureLog output
            readFile "result/build-artifact" `shouldReturn` "Hello, world!\n"
            exitCode output `shouldBe` ExitSuccess

          it "runs the environments setup steps" $ \onTestFailureLog -> do
            writeFile
              "garn.ts"
              [i|
                import * as garn from "#{repoDir}/ts/mod.ts"
                import * as nix from "#{repoDir}/ts/nix.ts"

                export const project = garn.mkProject(
                  { description: "" },
                  {
                    package: garn
                      .mkEnvironment(
                        undefined,
                        nix.nixStrLit`SETUP_VAR="hello from setup"`,
                      )
                      .build`
                        echo $SETUP_VAR > $out/build-artifact
                      `,
                  },
                )
              |]
            output <- runGarn ["build", "project"] "" repoDir Nothing
            onTestFailureLog output
            readFile "result/build-artifact" `shouldReturn` "hello from setup\n"
            exitCode output `shouldBe` ExitSuccess

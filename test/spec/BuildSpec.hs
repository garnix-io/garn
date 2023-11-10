{-# LANGUAGE QuasiQuotes #-}

module BuildSpec where

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Development.Shake (StdoutTrim (..), cmd)
import System.Directory
import System.Exit (ExitCode (..))
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils

wrap :: SpecWith ([String] -> IO ProcResult) -> Spec
wrap spec = do
  repoDir <- runIO getCurrentDirectory
  around_
    ( withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]
        . inTempDirectory
    )
    $ around
      ( \test -> onTestFailureLogger $ \onTestFailureLog -> do
          let runGarn' args = do
                output <- runGarn args "" repoDir Nothing
                onTestFailureLog output
                return output
          test runGarn'
      )
    $ spec

spec :: Spec
spec = do
  describe "build" $ do
    repoDir <- runIO getCurrentDirectory
    wrap $ do
      it "builds packages and creates a result link" $ \runGarn -> do
        writeHaskellProject repoDir
        _ <- runGarn ["build", "foo"]
        doesDirectoryExist "result" `shouldReturn` True
        StdoutTrim output <- cmd ("result/bin/garn-test" :: String)
        output `shouldBe` ("haskell test output" :: String)

      it "complains about packages that cannot be built" $ \runGarn -> do
        writeHaskellProject repoDir
        writeFile
          "Main.hs"
          [i|
            main :: IO ()
            main = "foo"
          |]
        output <- runGarn ["build", "foo"]
        stderr output `shouldContain` "Couldn't match type"
        exitCode output `shouldBe` ExitFailure 1

      it "allows to specify which sub-package to build" $ \runGarn -> do
        writeFile "garn.ts" $
          unindent
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts";

              export const project = garn.mkProject(
                { description: "" },
                {
                  foo: garn.build`echo 'foo' > $out/build-artifact`,
                  bar: garn.build`echo 'bar' > $out/build-artifact`,
                },
              );
            |]
        output <- runGarn ["build", "project.foo"]
        exitCode output `shouldBe` ExitSuccess
        readFile "result/build-artifact" `shouldReturn` "foo\n"
        output <- runGarn ["build", "project.bar"]
        exitCode output `shouldBe` ExitSuccess
        readFile "result/build-artifact" `shouldReturn` "bar\n"

      it "builds top-level packages" $ \runGarn -> do
        writeFile "garn.ts" $
          unindent
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts";

              export const p = garn.build`echo 'build output' > $out/build-artifact`;
            |]
        output <- runGarn ["build", "p"]
        exitCode output `shouldBe` ExitSuccess
        readFile "result/build-artifact" `shouldReturn` "build output\n"

      it "shows a nice help for packages" $ \runGarn -> do
        writeFile "garn.ts" $
          unindent
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts";

              export const project = garn.mkProject(
                { description: "" },
                {
                  short: garn.build`short command`,
                  longer: garn.build`
                    # this is some longer build script:
                    bla bla bla
                  `,
                },
              );
            |]
        output <- runGarn ["build"]
        stderr output `shouldContain` "project.short            Builds short command"
        stderr output `shouldContain` "project.longer           Builds # this is some longe..."

      describe ".build" $ do
        it "builds manually specified packages" $ \runGarn -> do
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
          output <- runGarn ["build", "project"]
          readFile "result/build-artifact" `shouldReturn` "build-content\n"
          exitCode output `shouldBe` ExitSuccess

        it "makes tools from the environment available" $ \runGarn -> do
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
                      .withDevTools([garn.mkPackage(nix.nixRaw`pkgs.hello`, "hello")])
                      .build`
                        hello > $out/build-artifact
                      `,
                },
              )
            |]
          output <- runGarn ["build", "project"]
          readFile "result/build-artifact" `shouldReturn` "Hello, world!\n"
          exitCode output `shouldBe` ExitSuccess

        it "runs the environments setup steps" $ \runGarn -> do
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
          output <- runGarn ["build", "project"]
          readFile "result/build-artifact" `shouldReturn` "hello from setup\n"
          exitCode output `shouldBe` ExitSuccess

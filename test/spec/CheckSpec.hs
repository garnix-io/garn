{-# LANGUAGE QuasiQuotes #-}

module CheckSpec where

import Control.Monad (forM_)
import Data.String.Interpolate
import System.Directory
import System.Exit (ExitCode (..))
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils

spec :: Spec
spec = do
  describe "check" $ do
    repoDir <- runIO getCurrentDirectory
    around_
      ( withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]
          . inTempDirectory
      )
      $ do
        it "runs manually added checks" $ do
          writeHaskellProject repoDir
          writeFile
            "Main.hs"
            [i|
              main :: IO ()
              main = return ()

              f :: [Int] -> [Int]
              f list = map (+ 1) list
            |]
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              const haskellBase = garn.haskell.mkHaskell({
                description: "mkHaskell-test",
                executable: "garn-test",
                compiler: "ghc94",
                src: "."
              }).withDevTools([garn.mkPackage(`pkgs.hlint`)]);

              export const haskell = {
                ...haskellBase,
                hlint: haskellBase.check`hlint *.hs`,
              };
            |]
          output <- runGarn ["check", "haskell"] "" repoDir Nothing
          stderr output `shouldContain` "Warning: Eta reduce"
        it "runs checks on source directories that ignore the flake.nix file" $ do
          writeHaskellProject repoDir
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              const haskellBase = garn.haskell.mkHaskell({
                description: "mkHaskell-test",
                executable: "garn-test",
                compiler: "ghc94",
                src: "."
              });

              export const haskell = {
                ...haskellBase,
                hlint: haskellBase.check`
                  ls
                  false
                `,
              };
            |]
          output <- runGarn ["check", "haskell"] "" repoDir Nothing
          stderr output `shouldNotContain` "flake.nix"
        it "supports running checks in the default environment" $ do
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const failing = garn.mkProject(
                { description: 'Failing Project' },
                {
                  check1: garn.check`echo ABC`,
                  check2: garn.check`echo DEF && false`,
                  check3: garn.check`echo GHI`,
                }
              );
            |]
          output <- runGarn ["check", "failing"] "" repoDir Nothing
          stderr output `shouldContain` "DEF"
          exitCode output `shouldBe` ExitFailure 1
        describe "exit-codes" $ do
          let testCases =
                [ ("passing", "true", ExitSuccess),
                  ("failing", "false", ExitFailure 1),
                  ("unknownCommand", "does-not-exist", ExitFailure 1),
                  ("failureBeforeOtherCommands", "false; true", ExitFailure 1),
                  ("negated", "! true", ExitFailure 1),
                  ("pipefail", "false | true", ExitFailure 1)
                ]
          forM_ testCases $ \(checkName, check :: String, expectedExitCode) -> do
            it ("reports exit-codes correctly for check '" <> checkName <> "'") $ do
              writeHaskellProject repoDir
              writeFile
                "garn.ts"
                [i|
                  import * as garn from "#{repoDir}/ts/mod.ts"

                  const haskellBase = garn.haskell.mkHaskell({
                    description: "mkHaskell-test",
                    executable: "garn-test",
                    compiler: "ghc94",
                    src: "."
                  });

                  export const haskell = {
                    ...haskellBase,
                    check: haskellBase.check`#{check}`,
                  };
                |]
              result <- runGarn ["check", "haskell"] "" repoDir Nothing
              putStrLn $ stdout result
              putStrLn $ stderr result
              stderr result `shouldNotContain` "Invalid argument"
              exitCode result `shouldBe` expectedExitCode

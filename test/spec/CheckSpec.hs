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
      . around onTestFailureLogger
      $ do
        it "runs manually added checks" $ \onTestFailureLog -> do
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
              import { nixRaw } from "#{repoDir}/ts/nix.ts";

              export const haskell = garn.haskell.mkHaskellProject({
                description: "mkHaskellProject-test",
                executable: "garn-test",
                compiler: "ghc94",
                src: "."
              })
                .withDevTools([garn.mkPackage(nixRaw`pkgs.hlint`)])
                .addCheck("hlint", "hlint *.hs");
            |]
          output <- runGarn ["check", "haskell"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldContain` "Warning: Eta reduce"

        it "runs checks on source directories that ignore the flake.nix file" $ \onTestFailureLog -> do
          writeHaskellProject repoDir
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const haskell = garn.haskell.mkHaskellProject({
                description: "mkHaskellProject-test",
                executable: "garn-test",
                compiler: "ghc94",
                src: "."
              })
                .addCheck("check")`
                  ls
                  false
                `;
            |]
          output <- runGarn ["check", "haskell"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldNotContain` "flake.nix"

        it "supports running checks in the default environment" $ \onTestFailureLog -> do
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const failing = garn.mkProject(
                { description: 'Failing Project' },
                {
                  check1: garn.check("echo ABC"),
                  check2: garn.check("echo DEF && false"),
                  check3: garn.check("echo GHI"),
                }
              );
            |]
          output <- runGarn ["check", "failing"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldContain` "DEF"
          exitCode output `shouldBe` ExitFailure 1

        it "supports running checks on projects containing a dep 'check'" $ \onTestFailureLog -> do
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"
              export const project = garn.mkProject({
                description: "",
                defaultEnvironment: garn.mkEnvironment(),
              }, {})
                .addExecutable("check", "true")
                .addCheck("test", "echo this works " + Date.now());
            |]
          output <- runGarn ["check", "project"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldContain` "this works"
          exitCode output `shouldBe` ExitSuccess

        it "does not error if there are spaces in the check key name" $ \onTestFailureLog -> do
          writeHaskellProject repoDir
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const myProject = garn.mkProject({
                description: "",
                defaultEnvironment: garn.mkEnvironment(),
              }, {}).addCheck("my check")`echo hello world && false`;
            |]
          output <- runGarn ["check", "myProject"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldContain` "hello world"
          exitCode output `shouldBe` ExitFailure 1

        it "allows to use backtick syntax" $ \onTestFailureLog -> do
          writeHaskellProject repoDir
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"
              import { nixRaw } from "#{repoDir}/ts/nix.ts";

              const hello: garn.Package = garn.mkPackage(nixRaw`pkgs.hello`);

              export const haskell = garn.mkProject(
                {
                  description: "",
                  defaultEnvironment: garn.emptyEnvironment,
                },
                {},
              ).addCheck("test-check")`${hello}/bin/hello; false`;
            |]
          output <- runGarn ["check", "haskell"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldContain` "error"
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
            it ("reports exit-codes correctly for check '" <> checkName <> "'") $ \onTestFailureLog -> do
              writeHaskellProject repoDir
              writeFile
                "garn.ts"
                [i|
                  import * as garn from "#{repoDir}/ts/mod.ts"

                  export const haskell = garn.haskell.mkHaskellProject({
                    description: "mkHaskellProject-test",
                    executable: "garn-test",
                    compiler: "ghc94",
                    src: "."
                  })
                    .addCheck("check")`#{check}`;
                |]
              output <- runGarn ["check", "haskell"] "" repoDir Nothing
              onTestFailureLog output
              stderr output `shouldNotContain` "Invalid argument"
              exitCode output `shouldBe` expectedExitCode

        it "runs *all* checks when no target given" $ \onTestFailureLog -> do
          writeHaskellProject repoDir
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const haskell = garn.mkProject(
                {
                  description: "mkHaskell-test",
                  defaultEnvironment: garn.emptyEnvironment,
                },
                {},
              )
                .addCheck("check")`echo first failure ; false`;

              export const other = garn.mkProject(
                {
                  description: "other",
                  defaultEnvironment: garn.emptyEnvironment,
                },
                {},
              )
                .addCheck("check")`echo second failure ; false`;
            |]
          output <- runGarn ["check"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldContain` "first failure"
          exitCode output `shouldBe` ExitFailure 1

          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const haskell = garn.mkProject(
                {
                  description: "mkHaskell-test",
                  defaultEnvironment: garn.emptyEnvironment,
                },
                {},
              )
                .addCheck("check")`echo first success`;

              export const other = garn.mkProject(
                {
                  description: "other",
                  defaultEnvironment: garn.emptyEnvironment,
                },
                {},
              )
                .addCheck("check")`echo second failure ; false`;
            |]
          output <- runGarn ["check"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldContain` "second failure"
          exitCode output `shouldBe` ExitFailure 1

        it "displays an error if `addCheck` is exported without its template literal" $ \onTestFailureLog -> do
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const badExport = garn.mkProject({
                description: "mkHaskell-test",
                defaultEnvironment: garn.emptyEnvironment,
              }, {})
                .addCheck("forgot-to-call-template-literal");
            |]
          output <- runGarn ["check"] "" repoDir Nothing
          onTestFailureLog output
          stderr output `shouldContain` "badExport exports the return type of \"addCheck\", but this is not the proper usage of addCheck."
          exitCode output `shouldBe` ExitFailure 1

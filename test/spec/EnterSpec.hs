{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module EnterSpec where

import Control.Lens (from, (<>~))
import Data.Aeson.Lens
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Vector.Generic.Lens (vector)
import Development.Shake (StdoutTrim (..), cmd)
import System.Directory
import System.Exit (ExitCode (..))
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment
import TestUtils

spec :: Spec
spec = do
  describe "enter" $ do
    repoDir <- runIO getCurrentDirectory
    around_
      ( withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]
          . inTempDirectory
      )
      $ do
        describe "withDevTools" $ do
          it "allows dev tools to be added to the dev shell" $ do
            writeHaskellProject repoDir
            writeFile "garn.ts" $
              unindent
                [i|
                  import { mkHaskellProject } from "#{repoDir}/ts/haskell/mod.ts"
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { nixRaw } from "#{repoDir}/ts/nix.ts";

                  export const foo = mkHaskellProject({
                    description: "mkHaskellProject-test",
                    executables: ["garn-test"],
                    compiler: "ghc94",
                    src: "."
                  })

                  export const bar = foo.withDevTools([mkPackage(nixRaw`pkgs.hello`, "hello")]);
                |]
            output <- runGarn ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"

          it "allows multiple dev tools to be added to the dev shell" $ do
            writeHaskellProject repoDir
            writeFile "garn.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { mkHaskellProject } from "#{repoDir}/ts/haskell/mod.ts"
                  import { nixRaw } from "#{repoDir}/ts/nix.ts";

                  export const foo = mkHaskellProject({
                    description: "mkHaskellProject-test",
                    executables: ["garn-test"],
                    compiler: "ghc94",
                    src: "."
                  })

                  export const bar = foo.withDevTools([
                    mkPackage(nixRaw`pkgs.hello`, "hello"),
                    mkPackage(nixRaw`pkgs.cowsay`, "cowsay"),
                  ]);
                |]
            output <- runGarn ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"
            output <- runGarn ["enter", "bar"] "which cowsay\nexit\n" repoDir Nothing
            stdout output `shouldStartWith` "/nix/store"

          it "does not destructively update the given package" $ onTestFailureLogger $ \onFailingTestLog -> do
            writeHaskellProject repoDir
            writeFile "garn.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { mkHaskellProject } from "#{repoDir}/ts/haskell/mod.ts"
                  import { nixRaw } from "#{repoDir}/ts/nix.ts";

                  export const foo = mkHaskellProject({
                    description: "mkHaskellProject-test",
                    executables: ["garn-test"],
                    compiler: "ghc94",
                    src: "."
                  })

                  export const bar = foo.withDevTools([mkPackage(nixRaw`pkgs.hello`, "hello")]);
                |]
            output <- runGarn ["enter", "foo"] "hello -g tool\nexit\n" repoDir Nothing
            onFailingTestLog output
            stderr output `shouldContain` "hello: command not found"

          it "can safely be used twice" $ do
            writeHaskellProject repoDir
            writeFile "garn.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { mkHaskellProject } from "#{repoDir}/ts/haskell/mod.ts"
                  import { nixRaw } from "#{repoDir}/ts/nix.ts";

                  export const foo = mkHaskellProject({
                    description: "mkHaskellProject-test",
                    executables: ["garn-test"],
                    compiler: "ghc94",
                    src: "."
                  })

                  export const bar = foo
                    .withDevTools([mkPackage(nixRaw`pkgs.hello`, "hello")])
                    .withDevTools([mkPackage(nixRaw`pkgs.cowsay`, "cowsay")]);
                |]
            output <- runGarn ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"
            output <- runGarn ["enter", "bar"] "which cowsay\nexit\n" repoDir Nothing
            stdout output `shouldStartWith` "/nix/store"

        it "has the right GHC version" $ do
          writeHaskellProject repoDir
          output <- runGarn ["enter", "foo"] "ghc --numeric-version\nexit\n" repoDir Nothing
          stdout output `shouldStartWith` "9.4"

        it "registers Haskell dependencies with ghc-pkg" $ do
          writeHaskellProject repoDir
          modifyPackageYaml $
            key "executables"
              . key "garn-test"
              . key "dependencies"
              . _Array
              . from vector
              <>~ ["string-conversions"]
          output <- runGarn ["enter", "foo"] "ghc-pkg list | grep string-conversions\nexit\n" repoDir Nothing
          dropWhile (== ' ') (stdout output) `shouldStartWith` "string-conversions"

        it "includes dependencies of simple packages that don't provide an 'env' attribute" $ do
          writeFile
            "garn.ts"
            [i|
              import { mkPackage } from "#{repoDir}/ts/package.ts"
              import { packageToEnvironment } from "#{repoDir}/ts/environment.ts"
              import { mkProject } from "#{repoDir}/ts/project.ts"
              import { nixRaw } from "#{repoDir}/ts/nix.ts"

              const pkg = mkPackage(
                nixRaw`
                  pkgs.stdenv.mkDerivation({
                    name = "blah";
                    src = ./.;
                    buildInputs = [ pkgs.hello ];
                  })
                `,
                "blah",
              );
              export const foo = mkProject(
                {
                  description: "description",
                  defaultEnvironment: packageToEnvironment(pkg, "."),
                },
                {},
              );
            |]
          output <- runGarn ["enter", "foo"] "hello\nexit\n" repoDir Nothing
          stdout output `shouldBe` "Hello, world!\n"

        it "starts the shell defined in $SHELL" $ do
          let shellTestCommand =
                [i|
                  if [[ -v BASH_VERSION ]]; then
                      echo -n "using bash"
                  else
                      if [[ -v ZSH_VERSION ]]; then
                          echo -n "using zsh"
                      else
                          echo -n "using unknown shell"
                      fi
                  fi
                  exit
                |]
          writeHaskellProject repoDir
          StdoutTrim userShell <- cmd ("which bash" :: String)
          output <-
            runGarn ["enter", "foo"] shellTestCommand repoDir $ Just userShell
          stdout output `shouldBe` "using bash"
          StdoutTrim userShell <- cmd ("which zsh" :: String)
          output <-
            runGarn ["enter", "foo"] shellTestCommand repoDir $ Just userShell
          stdout output `shouldBe` "using zsh"

        it "provides a message indicating the command succeeded" $ do
          writeHaskellProject repoDir
          output <- runGarn ["enter", "foo"] "exit\n" repoDir Nothing
          stderr output `shouldContain` "[garn] Entering foo shell. Type 'exit' to exit."

        it "provides a message indicating the shell exited" $ do
          writeHaskellProject repoDir
          output <- runGarn ["enter", "foo"] "exit\n" repoDir Nothing
          stderr output `shouldContain` "[garn] Exiting foo shell"

        it "fails when the shell cannot be entered" $ do
          writeHaskellProject repoDir
          removeFile "package.yaml"
          output <- runGarn ["enter", "foo"] "echo 'This cannot be executed.'\nexit\n" repoDir Nothing
          exitCode output `shouldBe` ExitFailure 1
          stderr output `shouldContain` "Found neither a .cabal file nor package.yaml. Exiting."

        describe "npm project" $ do
          it "puts node into the $PATH" $ do
            writeNpmFrontendProject repoDir
            output <- runGarn ["enter", "frontend"] "node --version\nexit\n" repoDir Nothing
            stdout output `shouldStartWith` "v18."
            output <- runGarn ["enter", "frontend"] "npm --version\nexit\n" repoDir Nothing
            stdout output `shouldStartWith` "9."

        describe "top-level environments" $ around onTestFailureLogger $ do
          it "allows entering top-level environments" $ \onTestFailureLog -> do
            writeFile
              "garn.ts"
              [i|
                import * as garn from "#{repoDir}/ts/mod.ts"

                const testTool = garn.build(`
                  mkdir -p $out/bin
                  echo 'echo test tool output' > $out/bin/test-tool
                  chmod +x $out/bin/test-tool
                `);

                export const topLevelEnv = garn.emptyEnvironment.withDevTools([testTool]);
              |]
            output <- runGarn ["enter", "topLevelEnv"] "test-tool\nexit\n" repoDir Nothing
            onTestFailureLog output
            stdout output `shouldBe` "test tool output\n"

          it "does not show a description for top-level environments" $ \onTestFailureLog -> do
            writeFile
              "garn.ts"
              [i|
                import * as garn from "#{repoDir}/ts/mod.ts"

                export const topLevelEnv = garn.emptyEnvironment;
              |]
            output <- runGarn ["enter", "--help"] "" repoDir Nothing
            onTestFailureLog output
            stdout output
              `shouldMatch` unindent
                [i|
                  Available commands:
                    topLevelEnv[ ]*$
                |]

        describe "--help" $ around onTestFailureLogger $ do
          it "allows adding descriptions to environments" $ \onTestFailureLog -> do
            writeFile
              "garn.ts"
              [i|
                import * as garn from "#{repoDir}/ts/mod.ts"

                export const topLevelEnv =
                  garn.emptyEnvironment
                    .setDescription("my test environment");
              |]
            output <- runGarn ["enter", "--help"] "" repoDir Nothing
            onTestFailureLog output
            stdout output
              `shouldMatch` unindent
                [i|
                  Available commands:
                    topLevelEnv[ ]*my test environment
                |]

          it "does not show (or allow entering) the default environment of projects" $ \onTestFailureLog -> do
            writeFile
              "garn.ts"
              [i|
                import * as garn from "#{repoDir}/ts/mod.ts"

                export const p = garn.mkProject(
                  {
                    description: "",
                    defaultEnvironment: garn.emptyEnvironment,
                  },
                  {},
                );
              |]
            output <- runGarn ["enter", "--help"] "" repoDir Nothing
            onTestFailureLog output
            stdout output `shouldNotContain` "p.defaultEnvironment"
            output <- runGarn ["enter", "p.defaultEnvironment"] "" repoDir Nothing
            onTestFailureLog output
            exitCode output `shouldBe` ExitFailure 1
            stderr output `shouldContain` "Invalid argument `p.defaultEnvironment'"

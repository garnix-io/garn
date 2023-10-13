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
                  import { mkHaskellProject } from "#{repoDir}/ts/haskell.ts"
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { nixRaw } from "#{repoDir}/ts/nix.ts";

                  export const foo = mkHaskellProject({
                    description: "mkHaskellProject-test",
                    executable: "garn-test",
                    compiler: "ghc94",
                    src: "."
                  })

                  export const bar = foo.withDevTools([mkPackage(nixRaw`pkgs.hello`)]);
                |]
            output <- runGarn ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"
          it "allows multiple dev tools to be added to the dev shell" $ do
            writeHaskellProject repoDir
            writeFile "garn.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { mkHaskellProject } from "#{repoDir}/ts/haskell.ts"
                  import { nixRaw } from "#{repoDir}/ts/nix.ts";

                  export const foo = mkHaskellProject({
                    description: "mkHaskellProject-test",
                    executable: "garn-test",
                    compiler: "ghc94",
                    src: "."
                  })

                  export const bar = foo.withDevTools([
                    mkPackage(nixRaw`pkgs.hello`),
                    mkPackage(nixRaw`pkgs.cowsay`),
                  ]);
                |]
            output <- runGarn ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"
            output <- runGarn ["enter", "bar"] "which cowsay\nexit\n" repoDir Nothing
            stdout output `shouldStartWith` "/nix/store"
          it "does not destructively update the given package" $ do
            writeHaskellProject repoDir
            writeFile "garn.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { mkHaskellProject } from "#{repoDir}/ts/haskell.ts"
                  import { nixRaw } from "#{repoDir}/ts/nix.ts";

                  export const foo = mkHaskellProject({
                    description: "mkHaskellProject-test",
                    executable: "garn-test",
                    compiler: "ghc94",
                    src: "."
                  })

                  export const bar = foo.withDevTools([mkPackage(nixRaw`pkgs.hello`)]);
                |]
            output <- runGarn ["enter", "foo"] "hello -g tool\nexit\n" repoDir Nothing
            stderr output `shouldContain` "hello: command not found"
          it "can safely be used twice" $ do
            writeHaskellProject repoDir
            writeFile "garn.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { mkHaskellProject } from "#{repoDir}/ts/haskell.ts"
                  import { nixRaw } from "#{repoDir}/ts/nix.ts";

                  export const foo = mkHaskellProject({
                    description: "mkHaskellProject-test",
                    executable: "garn-test",
                    compiler: "ghc94",
                    src: "."
                  })

                  export const bar = foo
                    .withDevTools([mkPackage(nixRaw`pkgs.hello`)])
                    .withDevTools([mkPackage(nixRaw`pkgs.cowsay`)]);
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

              const pkg = mkPackage(nixRaw`
                pkgs.stdenv.mkDerivation({
                  name = "blah";
                  src = ./.;
                  buildInputs = [ pkgs.hello ];
                })
              `);
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
          output <- runGarn ["enter", "foo"] "" repoDir Nothing
          stderr output `shouldContain` "[garn] Entering foo shell. Type 'exit' to exit."
        it "provides a message indicating the shell exited" $ do
          writeHaskellProject repoDir
          output <- runGarn ["enter", "foo"] "" repoDir Nothing
          stderr output `shouldContain` "[garn] Exiting foo shell"
        it "fails when the shell cannot be entered" $ do
          writeHaskellProject repoDir
          removeFile "package.yaml"
          output <- runGarn ["enter", "foo"] "echo 'This cannot be executed.'" repoDir Nothing
          exitCode output `shouldBe` ExitFailure 1
          stderr output `shouldContain` "Found neither a .cabal file nor package.yaml. Exiting."

        describe "npm project" $ do
          it "puts node into the $PATH" $ do
            writeNpmFrontendProject repoDir
            output <- runGarn ["enter", "frontend"] "node --version" repoDir Nothing
            stdout output `shouldStartWith` "v18."
            output <- runGarn ["enter", "frontend"] "npm --version" repoDir Nothing
            stdout output `shouldStartWith` "9."

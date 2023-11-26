{-# LANGUAGE QuasiQuotes #-}

module RunSpec where

import Control.Monad (forM_)
import Data.List (sort)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import System.Directory
import System.Exit (ExitCode (..))
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
  describe "run" $ withRunGarn $ do
    repoDir <- runIO getCurrentDirectory
    it "runs a simple Haskell program" $ \runGarn -> do
      writeHaskellProject repoDir
      output <- runGarn ["run", "foo.garn-test"]
      stdout output `shouldBe` "haskell test output\n"

    it "writes flake.{lock,nix}, but no other files" $ \runGarn -> do
      writeHaskellProject repoDir
      filesBefore <- listDirectory "."
      _ <- runGarn ["run", "foo.garn-test"]
      filesAfter <- sort <$> listDirectory "."
      filesAfter `shouldBe` sort (filesBefore ++ ["flake.lock", "flake.nix"])

    it "runs arbitrary executables" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const main = garn.shell("echo foobarbaz");
            |]
      output <- runGarn ["run", "main"]
      stdout output `shouldBe` "foobarbaz\n"
      exitCode output `shouldBe` ExitSuccess

    it "propagates the exit status of the child process" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const main = garn.shell`exit 23`;
            |]
      output <- runGarn ["run", "main"]
      stdout output `shouldBe` ""
      exitCode output `shouldBe` ExitFailure 23

    it "runs executables within an environment" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"
              import { nixRaw } from "#{repoDir}/ts/nix.ts";

              const myEnv = garn.mkEnvironment().withDevTools([garn.mkPackage(nixRaw`pkgs.hello`, "hello")]);
              export const main = myEnv.shell("hello");
            |]
      output <- runGarn ["run", "main"]
      stdout output `shouldBe` "Hello, world!\n"
      exitCode output `shouldBe` ExitSuccess

    it "fails informatively on syntax errors" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
          {askd "shyntax err"
        |]
      output <- runGarn ["run", "foo"]
      stderr output `shouldContain` "Error: Running garn.ts failed:"
      stderr output `shouldContain` "The module's source code could not be parsed"

    it "runs non-default executables within projects" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"
              export const project = garn.mkProject({
                description: "my project",
                defaultEnvironment: garn.emptyEnvironment,
              }, {}).addExecutable("hello", "echo Hello, world!");
            |]
      output <- runGarn ["run", "project.hello"]
      stdout output `shouldBe` "Hello, world!\n"
      exitCode output `shouldBe` ExitSuccess

    it "supports running executables on projects containing a dep 'shell'" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"
              export const project = garn.mkProject({
                description: "",
                defaultEnvironment: garn.mkEnvironment(),
              }, {})
                .addExecutable("shell", "true")
                .addExecutable("greet", "echo hello world");
            |]
      output <- runGarn ["run", "project.greet"]
      stdout output `shouldContain` "hello world"
      exitCode output `shouldBe` ExitSuccess

    it "allows specifying deeply nested executables" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"
              const a = garn.mkProject({
                description: "a",
                defaultExecutable: garn.shell("echo executable in a"),
              }, {});
              const b = garn.mkProject({
                description: "b",
              }, { a });
              export const c = garn.mkProject({
                description: "b",
              }, { b });
            |]
      output <- runGarn ["run", "c.b.a"]
      stdout output `shouldBe` "executable in a\n"
      exitCode output `shouldBe` ExitSuccess

    it "allows specifying argv to the executable" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const main = garn.shell('printf "%s,%s,%s"');
            |]
      output <- runGarn ["run", "main", "foo bar", "baz"]
      stdout output `shouldBe` "foo bar,baz,"
      exitCode output `shouldBe` ExitSuccess

    it "allows specifying options to the executable with --" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const main = garn.shell('printf "%s,%s,%s"');
            |]
      output <- runGarn ["run", "main", "--", "--bar", "--baz"]
      stdout output `shouldBe` "--bar,--baz,"
      exitCode output `shouldBe` ExitSuccess

    it "doesn’t format other Nix files" $ \runGarn -> do
      let unformattedNix =
            [i|
                      { ...
                        }
                  :       {
                    some              =     poorly
                  formatted nix;
                        }
                |]
      writeFile "unformatted.nix" unformattedNix
      writeHaskellProject repoDir
      _ <- runGarn ["run", "foo"]
      readFile "./unformatted.nix" `shouldReturn` unformattedNix

    it "forwards the user's tty" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const printTty = garn.shell("tty");
            |]
      output <- runGarn ["run", "printTty"]
      stdout output `shouldStartWith` "/dev/"
      exitCode output `shouldBe` ExitSuccess

    it "allows to use backtick syntax" $ \runGarn -> do
      writeFile
        "garn.ts"
        [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              const hello = garn.mkPackage(garn.nix.nixRaw`pkgs.hello`, "hello");

              export const main = garn.mkProject(
                {
                  description: "",
                  defaultEnvironment: garn.emptyEnvironment,
                },
                {},
              ).addExecutable("foo")`${hello}/bin/hello`;
            |]
      output <- runGarn ["run", "main.foo"]
      stdout output `shouldBe` "Hello, world!\n"

    describe "top-level executables" $ do
      it "shows top-level executables in the help" $ \runGarn -> do
        writeFile "garn.ts" $
          unindent
            [i|
                  import * as garn from "#{repoDir}/ts/mod.ts"

                  export const topLevelExecutable: garn.Executable = garn.shell("true");
                |]
        output <- runGarn ["run", "--help"]
        stdout output
          `shouldMatch` unindent
            [i|
                  Available commands:
                    topLevelExecutable.*
                |]

      describe "help of other subcommands" $ do
        let commands = ["build", "enter", "check"]
        forM_ commands $ \command -> do
          describe command $ do
            it "does not show top-level executables in the help" $ \runGarn -> do
              writeFile "garn.ts" $
                unindent
                  [i|
                        import * as garn from "#{repoDir}/ts/mod.ts"

                        export const topLevelExecutable: garn.Executable = garn.shell("true");
                      |]
              output <- runGarn [command, "--help"]
              stdout output `shouldNotContain` "topLevelExecutable"

    describe "top-level projects" $ do
      it "shows runnable projects in the help" $ \runGarn -> do
        writeFile "garn.ts" $
          unindent
            [i|
                  import * as garn from "#{repoDir}/ts/mod.ts"

                  export const myProject = garn.mkProject({
                    description: "a runnable project",
                    defaultExecutable: garn.shell("echo runnable"),
                  }, {});
                |]
        output <- runGarn ["run", "--help"]
        stdout output
          `shouldMatch` unindent
            [i|
                  Available commands:
                    myProject.*
                |]

      it "does not show non-runnable projects in the help" $ \runGarn -> do
        writeFile "garn.ts" $
          unindent
            [i|
                  import * as garn from "#{repoDir}/ts/mod.ts"

                  export const myProject = garn.mkProject({
                    description: "not runnable",
                  }, {});
                |]
        output <- runGarn ["run", "--help"]
        stdout output `shouldNotContain` "myProject"

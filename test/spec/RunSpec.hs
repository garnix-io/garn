{-# LANGUAGE QuasiQuotes #-}

module RunSpec where

import Control.Monad (forM_)
import Data.List (sort)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils
  ( ProcResult (..),
    onTestFailureLogger,
    runGarnInDir,
    shouldMatch,
    writeHaskellProject,
  )
import Prelude hiding (writeFile)
import qualified Prelude

spec :: Spec
spec =
  describe "run" $ withContext $ do
    repoDir <- runIO getCurrentDirectory
    it "runs a simple Haskell program" $ \context -> do
      writeHaskellProject repoDir (Just (tempDir context))
      output <- runGarn context ["run", "foo"]
      stdout output `shouldBe` "haskell test output\n"

    it "writes flake.{lock,nix}, but no other files" $ \context -> do
      writeHaskellProject repoDir (Just (tempDir context))
      filesBefore <- listDirectory (tempDir context)
      _ <- runGarn context ["run", "foo"]
      filesAfter <- sort <$> listDirectory (tempDir context)
      filesAfter `shouldBe` sort (filesBefore ++ ["flake.lock", "flake.nix"])

    it "runs arbitrary executables" $ \context -> do
      writeFile
        context
        "garn.ts"
        [i|
          import * as garn from "#{repoDir}/ts/mod.ts"

          export const main = garn.shell("echo foobarbaz");
        |]
      output <- runGarn context ["run", "main"]
      stdout output `shouldBe` "foobarbaz\n"
      exitCode output `shouldBe` ExitSuccess

    it "propagates the exit status of the child process" $ \context -> do
      writeFile
        context
        "garn.ts"
        [i|
          import * as garn from "#{repoDir}/ts/mod.ts"

          export const main = garn.shell`exit 23`;
        |]
      output <- runGarn context ["run", "main"]
      stdout output `shouldBe` ""
      exitCode output `shouldBe` ExitFailure 23

    it "runs executables within an environment" $ \context -> do
      writeFile
        context
        "garn.ts"
        [i|
          import * as garn from "#{repoDir}/ts/mod.ts"
          import { nixRaw } from "#{repoDir}/ts/nix.ts";

          const myEnv = garn.mkEnvironment().withDevTools([garn.mkPackage(nixRaw`pkgs.hello`, "hello")]);
          export const main = myEnv.shell("hello");
        |]
      output <- runGarn context ["run", "main"]
      stdout output `shouldBe` "Hello, world!\n"
      exitCode output `shouldBe` ExitSuccess

    it "runs non-default executables within projects" $ \context -> do
      writeFile
        context
        "garn.ts"
        [i|
          import * as garn from "#{repoDir}/ts/mod.ts"
          export const project = garn.mkProject({
            description: "my project",
            defaultEnvironment: garn.emptyEnvironment,
          }, {}).addExecutable("hello", "echo Hello, world!");
        |]
      output <- runGarn context ["run", "project.hello"]
      stdout output `shouldBe` "Hello, world!\n"
      exitCode output `shouldBe` ExitSuccess

    it "supports running executables on projects containing a dep 'shell'" $ \context -> do
      writeFile
        context
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
      output <- runGarn context ["run", "project.greet"]
      stdout output `shouldContain` "hello world"
      exitCode output `shouldBe` ExitSuccess

    it "allows specifying deeply nested executables" $ \context -> do
      writeFile
        context
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
      output <- runGarn context ["run", "c.b.a"]
      stdout output `shouldBe` "executable in a\n"
      exitCode output `shouldBe` ExitSuccess

    it "allows specifying argv to the executable" $ \context -> do
      writeFile
        context
        "garn.ts"
        [i|
          import * as garn from "#{repoDir}/ts/mod.ts"

          export const main = garn.shell('printf "%s,%s,%s"');
        |]
      output <- runGarn context ["run", "main", "foo bar", "baz"]
      stdout output `shouldBe` "foo bar,baz,"
      exitCode output `shouldBe` ExitSuccess

    it "doesn’t format other Nix files" $ \context -> do
      let unformattedNix =
            [i|
                  { ...
                    }
              :       {
                some              =     poorly
              formatted nix;
                    }
            |]
      writeFile context "unformatted.nix" unformattedNix
      writeHaskellProject repoDir (Just (tempDir context))
      _ <- runGarn context ["run", "foo"]
      readFile (tempDir context </> "./unformatted.nix")
        `shouldReturn` unformattedNix

    it "forwards the user's tty" $ \context -> do
      writeFile
        context
        "garn.ts"
        [i|
          import * as garn from "#{repoDir}/ts/mod.ts"

          export const printTty = garn.shell("tty");
        |]
      output <- runGarn context ["run", "printTty"]
      stdout output `shouldStartWith` "/dev/"
      exitCode output `shouldBe` ExitSuccess

    it "allows to use backtick syntax" $ \context -> do
      writeFile
        context
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
      output <- runGarn context ["run", "main.foo"]
      stdout output `shouldBe` "Hello, world!\n"

    describe "top-level executables" $ do
      it "shows top-level executables in the help" $ \context -> do
        writeFile context "garn.ts" $
          unindent
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const topLevelExecutable: garn.Executable = garn.shell("true");
            |]
        output <- runGarn context ["run", "--help"]
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
            it "does not show top-level executables in the help" $ \context -> do
              writeFile context "garn.ts" $
                unindent
                  [i|
                    import * as garn from "#{repoDir}/ts/mod.ts"

                    export const topLevelExecutable: garn.Executable = garn.shell("true");
                  |]
              output <- runGarn context [command, "--help"]
              stdout output `shouldNotContain` "topLevelExecutable"

    describe "top-level projects" $ do
      it "shows runnable projects in the help" $ \context -> do
        writeFile context "garn.ts" $
          unindent
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const myProject = garn.mkProject({
                description: "a runnable project",
                defaultExecutable: garn.shell("echo runnable"),
              }, {});
            |]
        output <- runGarn context ["run", "--help"]
        stdout output
          `shouldMatch` unindent
            [i|
              Available commands:
                myProject.*
            |]

      it "does not show non-runnable projects in the help" $ \context -> do
        writeFile context "garn.ts" $
          unindent
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const myProject = garn.mkProject({
                description: "not runnable",
              }, {});
            |]
        output <- runGarn context ["run", "--help"]
        stdout output `shouldNotContain` "myProject"

data Context = Context
  { repoDir :: FilePath,
    tempDir :: FilePath,
    onTestFailureLog :: ProcResult -> IO ()
  }

runGarn :: Context -> [String] -> IO ProcResult
runGarn context =
  \args -> do
    output <- runGarnInDir (tempDir context) args "" (repoDir context) Nothing
    onTestFailureLog context output
    pure output

writeFile :: Context -> FilePath -> String -> IO ()
writeFile context = \path content ->
  Prelude.writeFile (tempDir context </> path) content

withContext :: SpecWith Context -> Spec
withContext test = do
  repoDir <- runIO getCurrentDirectory
  around
    ( \test ->
        withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")] $
          withSystemTempDirectory "garn-test" $ \tempDir ->
            onTestFailureLogger $ \onTestFailureLog ->
              test $
                Context
                  { repoDir,
                    tempDir,
                    onTestFailureLog
                  }
    )
    test

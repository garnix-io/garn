{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GarnerSpec where

import Control.Exception (bracket, catch)
import Control.Lens (from, (<>~))
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.Char (isSpace)
import Data.List (dropWhileEnd, sort)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Vector.Generic.Lens (vector)
import qualified Data.Yaml as Yaml
import Development.Shake (StdoutTrim (..), cmd)
import Garner
import System.Directory
import System.Environment (withArgs)
import System.Exit (ExitCode (..))
import System.IO (Handle, IOMode (..), withFile)
import qualified System.IO as Sys
import System.IO.Silently (hCapture, hCapture_)
import System.IO.Temp
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory (inTempDirectory)
import Test.Mockery.Environment (withModifiedEnvironment)
import Text.Regex.PCRE.Heavy (compileM, (=~))

spec :: Spec
spec = do
  repoDir <- runIO getCurrentDirectory

  around_ (withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]) $ do
    describe "garner" $ around_ inTempDirectory $ do
      describe "--help" $ do
        it "lists available commands" $ do
          output <- runGarner ["--help"] "" repoDir Nothing
          stdout output
            `shouldMatch` unindent
              [i|
                Available commands:
                  init.*
              |]
          writeFile "garner.ts" ""
          output <- runGarner ["--help"] "" repoDir Nothing
          stdout output
            `shouldMatch` unindent
              [i|
                Available commands:
                  run.*
                  enter.*
                  gen.*
                  check.*
              |]
        it "lists unavailable commands" $ do
          output <- runGarner ["--help"] "" repoDir Nothing
          stdout output
            `shouldMatch` unindent
              [i|
                Unavailable commands:
                  run
                  enter
                  gen
                  check
              |]
          writeFile "garner.ts" ""
          output <- runGarner ["--help"] "" repoDir Nothing
          stdout output
            `shouldMatch` unindent
              [i|
                Unavailable commands:
                  init
              |]

      describe "run" $ do
        it "runs a simple Haskell program" $ do
          writeHaskellProject repoDir
          output <- runGarner ["run", "foo"] "" repoDir Nothing
          stdout output `shouldBe` "haskell test output\n"
        it "writes flake.{lock,nix}, but no other files" $ do
          writeHaskellProject repoDir
          filesBefore <- listDirectory "."
          _ <- runGarner ["run", "foo"] "" repoDir Nothing
          filesAfter <- sort <$> listDirectory "."
          filesAfter `shouldBe` sort (filesBefore ++ ["flake.lock", "flake.nix"])
        it "doesn’t format other Nix files" $ do
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
          _ <- runGarner ["run", "foo"] "" repoDir Nothing
          readFile "./unformatted.nix" `shouldReturn` unformattedNix

      describe "enter" $ do
        describe "withDevTools" $ do
          it "allows dev tools to be added to the dev shell" $ do
            writeHaskellProject repoDir
            writeFile "garner.ts" $
              unindent
                [i|
                  import { mkHaskell } from "#{repoDir}/ts/haskell.ts"
                  import { mkPackage } from "#{repoDir}/ts/package.ts"

                  export const foo = mkHaskell({
                    description: "mkHaskell-test",
                    executable: "garner-test",
                    compiler: "ghc94",
                    src: "."
                  })
                  const hello = mkPackage(`pkgs.hello`)

                  export const bar = foo.withDevTools([hello]);
                |]
            output <- runGarner ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"
          it "allows multiple dev tools to be added to the dev shell" $ do
            writeHaskellProject repoDir
            writeFile "garner.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

                  export const foo = mkHaskell({
                    description: "mkHaskell-test",
                    executable: "garner-test",
                    compiler: "ghc94",
                    src: "."
                  })

                  const hello = mkPackage(`pkgs.hello`);

                  const cowsay = mkPackage(`pkgs.cowsay`);

                  export const bar = foo.withDevTools([hello, cowsay]);
                |]
            output <- runGarner ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"
            output <- runGarner ["enter", "bar"] "which cowsay\nexit\n" repoDir Nothing
            stdout output `shouldStartWith` "/nix/store"
          it "does not destructively update the given package" $ do
            writeHaskellProject repoDir
            writeFile "garner.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

                  export const foo = mkHaskell({
                    description: "mkHaskell-test",
                    executable: "garner-test",
                    compiler: "ghc94",
                    src: "."
                  })

                  const hello = mkPackage(`pkgs.hello`);

                  export const bar = foo.withDevTools([hello]);
                |]
            output <- runGarner ["enter", "foo"] "hello -g tool\nexit\n" repoDir Nothing
            stderr output `shouldContain` "hello: command not found"
          it "can safely be used twice" $ do
            writeHaskellProject repoDir
            writeFile "garner.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/package.ts"
                  import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

                  export const foo = mkHaskell({
                    description: "mkHaskell-test",
                    executable: "garner-test",
                    compiler: "ghc94",
                    src: "."
                  })

                  const hello = mkPackage(`pkgs.hello`);

                  const cowsay = mkPackage(`pkgs.cowsay`);

                  export const bar = foo.withDevTools([hello]).withDevTools([cowsay]);
                |]
            output <- runGarner ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"
            output <- runGarner ["enter", "bar"] "which cowsay\nexit\n" repoDir Nothing
            stdout output `shouldStartWith` "/nix/store"
        it "has the right GHC version" $ do
          writeHaskellProject repoDir
          output <- runGarner ["enter", "foo"] "ghc --numeric-version\nexit\n" repoDir Nothing
          stdout output `shouldStartWith` "9.4"
        it "registers Haskell dependencies with ghc-pkg" $ do
          writeHaskellProject repoDir
          modifyPackageYaml $
            key "executables"
              . key "garner-test"
              . key "dependencies"
              . _Array
              . from vector
              <>~ ["string-conversions"]
          output <- runGarner ["enter", "foo"] "ghc-pkg list | grep string-conversions\nexit\n" repoDir Nothing
          dropWhile (== ' ') (stdout output) `shouldStartWith` "string-conversions"
        it "includes dependencies of simple packages that don't provide an 'env' attribute" $ do
          writeFile
            "garner.ts"
            [i|
              import { mkPackage } from "#{repoDir}/ts/package.ts"
              import { packageToEnvironment } from "#{repoDir}/ts/environment.ts"
              import { mkProject } from "#{repoDir}/ts/project.ts"

              const pkg = mkPackage(`
                pkgs.stdenv.mkDerivation({
                  name = "blah";
                  src = ./.;
                  buildInputs = [ pkgs.hello ];
                })
              `);
              export const foo = mkProject(
                "description",
                { devShell: packageToEnvironment(pkg) },
                { defaults: { environment: "devShell" } }
              );
            |]
          output <- runGarner ["enter", "foo"] "hello\nexit\n" repoDir Nothing
          stdout output `shouldBe` "Hello, world!\n"
        it "starts the shell defined in $SHELL" $ do
          writeHaskellProject repoDir
          StdoutTrim userShell <- cmd ("which bash" :: String)
          output <-
            runGarner ["enter", "foo"] shellTestCommand repoDir $ Just userShell
          stdout output `shouldBe` "using bash"
          StdoutTrim userShell <- cmd ("which zsh" :: String)
          output <-
            runGarner ["enter", "foo"] shellTestCommand repoDir $ Just userShell
          stdout output `shouldBe` "using zsh"
        it "provides a message indicating the command succeeded" $ do
          writeHaskellProject repoDir
          output <- runGarner ["enter", "foo"] "" repoDir Nothing
          stderr output `shouldContain` "[garner] Entering foo shell. Type 'exit' to exit."
        it "provides a message indicating the shell exited" $ do
          writeHaskellProject repoDir
          output <- runGarner ["enter", "foo"] "" repoDir Nothing
          stderr output `shouldContain` "[garner] Exiting foo shell"

        describe "npm project" $ do
          it "puts node into the $PATH" $ do
            writeNpmFrontendProject repoDir
            output <- runGarner ["enter", "frontend"] "node --version" repoDir Nothing
            stdout output `shouldStartWith` "v18."
            output <- runGarner ["enter", "frontend"] "npm --version" repoDir Nothing
            stdout output `shouldStartWith` "9."

      describe "check" $ do
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
            "garner.ts"
            [i|
                import * as garner from "#{repoDir}/ts/mod.ts"

                const haskellBase = garner.haskell.mkHaskell({
                  description: "mkHaskell-test",
                  executable: "garner-test",
                  compiler: "ghc94",
                  src: "."
                }).withDevTools([garner.mkPackage(`pkgs.hlint`)])

                export const haskell = {
                  ...haskellBase,
                  hlint: haskellBase.check`hlint *.hs`,
                };
            |]

          output <- runGarner ["check", "haskell"] "" repoDir Nothing
          stderr output `shouldContain` "Warning: Eta reduce"

      describe "init" $ do
        it "uses the provided init function if there is one" $ do
          writeFile
            "garner.cabal"
            [i|
              name: garner
              version: 0.0.1
            |]
          output <- runGarner ["init"] "" repoDir Nothing
          stderr output `shouldBe` "[garner] Creating a garner.ts file\n"
          readFile "garner.ts"
            `shouldReturn` dropWhileEnd
              isSpace
              ( unindent
                  [i|
                    import * as garner from "http://localhost:8777/mod.ts"

                    export const garner = garner.haskell.mkHaskell({
                      description: "",
                      executable: "",
                      compiler: "ghc94",
                      src: "."
                    })
                  |]
              )
        it "logs unexpected errors" $ do
          writeFile "garner.cabal" [i| badCabalfile |]
          output <- runGarner ["init"] "" repoDir Nothing
          stderr output
            `shouldBe` unindent
              [i|
                [garner] Creating a garner.ts file
                [garner] Found but could not parse cabal file
              |]

    -- TODO: Golden tests currently can’t be integrated with the other test cases
    --       because stackbuilders/hspec-golden#40. The case below shows the
    --       effect that @`around_` `inTempDirectory`@ _should_ have.
    describe "garner-golden" $ do
      describe "run" $ do
        it "generates formatted flakes" $ do
          inTempDirectory $ do
            writeHaskellProject repoDir
            _ <- runGarner ["run", "foo"] "" repoDir Nothing
            flake <- readFile "./flake.nix"
            pure $ defaultGolden "generates_formatted_flakes" flake

modifyPackageYaml :: (Aeson.Value -> Aeson.Value) -> IO ()
modifyPackageYaml modifier = do
  decoded <- Yaml.decodeFileThrow "package.yaml"
  Yaml.encodeFile "package.yaml" $ modifier decoded

modifyPackageJson :: (Aeson.Value -> Aeson.Value) -> IO ()
modifyPackageJson modifier = do
  maybeDecoded <- Aeson.decodeFileStrict "package.json"
  case maybeDecoded of
    Nothing -> error "could not decode package.json"
    Just decoded -> Aeson.encodeFile "package.json" $ modifier decoded

writeHaskellProject :: FilePath -> IO ()
writeHaskellProject repoDir = do
  writeFile
    "garner.ts"
    [i|
      import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

      export const foo = mkHaskell({
        description: "mkHaskell-test",
        executable: "garner-test",
        compiler: "ghc94",
        src: "."
      })
    |]
  writeFile
    "Main.hs"
    [i|
      main :: IO ()
      main = putStrLn "haskell test output"
    |]
  writeFile
    "package.yaml"
    [i|
      executables:
        garner-test:
          main: Main.hs
          dependencies:
           - base
    |]

writeNpmFrontendProject :: FilePath -> IO ()
writeNpmFrontendProject repoDir = do
  writeFile
    "garner.ts"
    [i|
      import { mkNpmFrontend } from "#{repoDir}/ts/typescript.ts"

      export const frontend = mkNpmFrontend({
        description: "mkNpmFrontend-test",
        src: ".",
        nodeVersion: "18",
        testCommand: "",
      })
    |]
  writeFile
    "package.json"
    [i|
      {
        "name": "frontend",
        "version": "0.0.1",
        "scripts": {
          "start": "echo running"
        }
      }
    |]
  writeFile
    "package-lock.json"
    [i|
      {
        "name": "foo",
        "version": "1.0.0",
        "lockfileVersion": 2,
        "requires": true,
        "packages": {
          "": {
            "name": "foo",
            "version": "1.0.0",
            "license": "ISC"
          }
        }
      }
    |]

runGarner :: (HasCallStack) => [String] -> String -> FilePath -> Maybe FilePath -> IO ProcResult
runGarner args stdin repoDir shell = do
  userShell <- maybe (fromStdoutTrim <$> cmd ("which bash" :: String)) pure shell
  (stderr, stdout) <- hCapture [Sys.stderr] $
    hCapture_ [Sys.stdout] $
      withTempFile $ \stdin ->
        withArgs args $ do
          let env =
                Env
                  { stdin,
                    userShell,
                    tsRunnerFilename = repoDir <> "/ts/runner.ts",
                    initFileName = repoDir <> "/ts/init.ts"
                  }
          let go = do
                options <- readOptionsAndConfig env
                runWith env options
          go `catch` \(_ :: ExitCode) -> pure ()
  return $ ProcResult {..}
  where
    withTempFile :: (Handle -> IO a) -> IO a
    withTempFile action =
      bracket
        (writeSystemTempFile "garner-test-stdin" stdin)
        removeFile
        (\file -> withFile file ReadMode action)

data ProcResult = ProcResult
  { stdout :: String,
    stderr :: String
  }
  deriving (Show)

shellTestCommand :: String
shellTestCommand =
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
  |]

shouldMatch :: (HasCallStack) => String -> String -> Expectation
shouldMatch actual expected = case compileM (cs expected) [] of
  Left err -> expectationFailure $ "invalid regex: " <> show err
  Right regex ->
    unless (actual =~ regex) $
      expectationFailure $
        "expected " <> actual <> " to match regex " <> show expected

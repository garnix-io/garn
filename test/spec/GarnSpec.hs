{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GarnSpec where

import Control.Exception (bracket, catch)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import qualified Data.Yaml as Yaml
import Development.Shake (StdoutTrim (..), cmd)
import Garn
import System.Directory
import System.Environment (withArgs)
import System.Exit (ExitCode (..))
import System.IO (Handle, IOMode (..), withFile)
import qualified System.IO as Sys
import System.IO.Silently (hCapture)
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
    describe "garn" $ around_ inTempDirectory $ do
      describe "--help" $ do
        it "lists available commands" $ do
          output <- runGarn ["--help"] "" repoDir Nothing
          stdout output
            `shouldMatch` unindent
              [i|
                Available commands:
                  init.*
              |]
          writeFile "garn.ts" [i|import "#{repoDir}/ts/mod.ts"|]
          output <- runGarn ["--help"] "" repoDir Nothing
          stdout output
            `shouldMatch` unindent
              [i|
                Available commands:
                  build.*
                  run.*
                  enter.*
                  gen.*
                  check.*
              |]
        it "lists unavailable commands" $ do
          output <- runGarn ["--help"] "" repoDir Nothing
          stdout output
            `shouldMatch` unindent
              [i|
                Unavailable commands:
                  build
                  run
                  enter
                  gen
                  check
              |]
          writeFile "garn.ts" [i|import "#{repoDir}/ts/mod.ts"|]
          output <- runGarn ["--help"] "" repoDir Nothing
          stdout output
            `shouldMatch` unindent
              [i|
                Unavailable commands:
                  init
              |]

    -- TODO: Golden tests currently can’t be integrated with the other test cases
    --       because stackbuilders/hspec-golden#40. The case below shows the
    --       effect that @`around_` `inTempDirectory`@ _should_ have.
    describe "garn-golden" $ do
      describe "run" $ do
        it "generates formatted flakes" $ do
          inTempDirectory $ do
            writeHaskellProject repoDir
            _ <- runGarn ["run", "foo"] "" repoDir Nothing
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
    "garn.ts"
    [i|
      import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

      export const foo = mkHaskell({
        description: "mkHaskell-test",
        executable: "garn-test",
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
        garn-test:
          main: Main.hs
          dependencies:
           - base
    |]

writeNpmFrontendProject :: FilePath -> IO ()
writeNpmFrontendProject repoDir = do
  writeFile
    "garn.ts"
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

runGarn :: (HasCallStack) => [String] -> String -> FilePath -> Maybe FilePath -> IO ProcResult
runGarn args stdin repoDir shell = do
  userShell <- maybe (fromStdoutTrim <$> cmd ("which bash" :: String)) pure shell
  (stderr, (stdout, exitCode)) <- hCapture [Sys.stderr] $
    hCapture [Sys.stdout] $
      withTempFile $ \stdin ->
        withArgs args $ do
          let env =
                Env
                  { stdin,
                    userShell,
                    initFileName = repoDir <> "/ts/internal/init.ts"
                  }
          let go = do
                options <- readOptionsAndConfig
                runWith env options
                return ExitSuccess
          go `catch` \(e :: ExitCode) -> pure e
  return $
    ProcResult
      { stdout,
        stderr,
        exitCode
      }
  where
    withTempFile :: (Handle -> IO a) -> IO a
    withTempFile action =
      bracket
        (writeSystemTempFile "garn-test-stdin" stdin)
        removeFile
        (\file -> withFile file ReadMode action)

data ProcResult = ProcResult
  { stdout :: String,
    stderr :: String,
    exitCode :: ExitCode
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

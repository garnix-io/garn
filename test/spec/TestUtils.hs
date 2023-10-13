{-# LANGUAGE QuasiQuotes #-}

module TestUtils where

import Control.Concurrent
import Control.Exception (SomeException, bracket, catch, throwIO)
import qualified Data.Aeson as Aeson
import Data.String.Interpolate
import qualified Data.Yaml as Yaml
import Development.Shake (StdoutTrim (..), cmd)
import Garn
import System.Directory
import System.Environment (withArgs)
import System.Exit
import System.IO (Handle, IOMode (..), hPutStrLn, withFile)
import qualified System.IO as Sys
import System.IO.Silently (hCapture)
import System.IO.Temp
import Test.Hspec

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
      import { mkNpmProject } from "#{repoDir}/ts/javascript.ts"

      export const frontend = mkNpmProject({
        description: "mkNpmProject-test",
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

onTestFailureLogger :: ((ProcResult -> IO ()) -> IO a) -> IO a
onTestFailureLogger test = do
  mvar <- newMVar []
  let log :: ProcResult -> IO ()
      log x = do
        modifyMVar_ mvar $ \acc ->
          return $
            acc
              ++ [ "exitcode: " <> show (exitCode x),
                   "=======",
                   "stdout: \n" <> stdout x,
                   "=======",
                   "stderr: \n" <> stderr x,
                   "======="
                 ]
  test log
    `catch` ( \(e :: SomeException) -> do
                logs <- readMVar mvar
                hPutStrLn Sys.stderr (unlines logs)
                throwIO e
            )

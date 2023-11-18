{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module TestUtils where

import Control.Concurrent
import Control.Concurrent.Async (waitEitherCatch, withAsync)
import Control.Exception (SomeException, bracket, catch, throwIO)
import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as Aeson
import Data.String.Conversions (cs)
import Data.String.Interpolate
import qualified Data.Yaml as Yaml
import Development.Shake (CmdOption (EchoStdout), Exit (Exit), StdoutTrim (..), cmd)
import Development.Shake.Command (CmdOption (Cwd), Stdout (Stdout))
import Garn
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Environment (withArgs)
import System.Exit
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, hClose, hPutStr, hPutStrLn)
import qualified System.IO as Sys
import System.IO.Silently (hCapture)
import System.Posix (fdToHandle, openPseudoTerminal)
import System.Process (ProcessHandle, interruptProcessGroupOf, waitForProcess)
import Test.Hspec
import Test.Mockery.Directory (inTempDirectory)
import Text.Regex.PCRE.Heavy (compileM, (=~))

shouldMatch :: (HasCallStack) => String -> String -> Expectation
shouldMatch actual expected = case compileM (cs expected) [] of
  Left err -> expectationFailure $ "invalid regex: " <> show err
  Right regex ->
    unless (actual =~ regex) $
      expectationFailure $
        "expected " <> actual <> " to match regex " <> show expected

writeHaskellProject :: FilePath -> IO ()
writeHaskellProject repoDir = do
  writeFile
    "garn.ts"
    [i|
      import { mkHaskellProject } from "#{repoDir}/ts/haskell/mod.ts"

      export const foo = mkHaskellProject({
        description: "mkHaskellProject-test",
        executables: ["garn-test"],
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
      import { mkNpmProject } from "#{repoDir}/ts/javascript/mod.ts"

      export const frontend = mkNpmProject({
        description: "mkNpmProject-test",
        src: ".",
        nodeVersion: "18",
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
    hCapture [Sys.stdout] $ do
      withStdinTty stdin $ \stdin -> do
        withArgs args $ do
          let env =
                Env
                  { stdin,
                    userShell,
                    initFileName = repoDir <> "/ts/internal/init.ts"
                  }
          let go = do
                run env
                return ExitSuccess
          go `catch` \(e :: ExitCode) -> pure e
  return $
    ProcResult
      { stdout,
        stderr,
        exitCode
      }

withStdinTty :: String -> (Handle -> IO a) -> IO a
withStdinTty stdinText action = do
  bracket setup teardown (action . snd)
  where
    setup = do
      (write, read) <- openPseudoTerminal
      stdinWriteHandle <- fdToHandle write
      stdinReadHandle <- fdToHandle read
      hPutStr stdinWriteHandle stdinText
      pure (stdinWriteHandle, stdinReadHandle)
    teardown (write, read) = do
      hClose write
      hClose read

data ProcResult = ProcResult
  { stdout :: String,
    stderr :: String,
    exitCode :: ExitCode
  }
  deriving (Show)

modifyPackageYaml :: (Aeson.Value -> Aeson.Value) -> IO ()
modifyPackageYaml = modifyYamlFile "package.yaml"

modifyYamlFile :: FilePath -> (Aeson.Value -> Aeson.Value) -> IO ()
modifyYamlFile file modifier = do
  decoded <- Yaml.decodeFileThrow file
  Aeson.encodeFile file $ modifier decoded

modifyPackageJson :: (Aeson.Value -> Aeson.Value) -> IO ()
modifyPackageJson modifier = do
  maybeDecoded <- Aeson.decodeFileStrict "package.json"
  case maybeDecoded of
    Nothing -> error "could not decode package.json"
    Just decoded -> Aeson.encodeFile "package.json" $ modifier decoded

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

raceCatch :: IO a -> IO b -> IO (Either (Either SomeException a) (Either SomeException b))
raceCatch left right =
  withAsync left $ \a ->
    withAsync right $ \b ->
      waitEitherCatch a b

withCmd :: IO ProcessHandle -> IO a -> IO a
withCmd cmd action = do
  result <-
    raceCatch
      (bracket cmd interruptProcessGroupOf waitForProcess)
      action
  case result of
    Left result -> do
      expectationFailure $ "cmd exited before action: " <> show result
      pure undefined
    Right (Right a) -> pure a
    Right (Left exception) -> throwIO exception

withFileServer :: IO () -> IO ()
withFileServer action = do
  running <- isRunning
  if running
    then action
    else withCmd (cmd "just fileserver") $ do
      waitUntilRunning
      action
  where
    waitUntilRunning = do
      running <- isRunning
      when (not running) $ do
        threadDelay 100000
        waitUntilRunning

    isRunning = do
      Exit c <-
        cmd
          "curl --silent localhost:8777/base.ts"
          (EchoStdout False)
      pure $ c == ExitSuccess

inExampleCopy :: FilePath -> FilePath -> IO a -> IO a
inExampleCopy repoDir exampleDir action = do
  Stdout (words -> files) <- cmd (Cwd (repoDir </> "examples" </> exampleDir)) "git ls-files"
  inTempDirectory $ do
    forM_ files $ \file -> do
      createDirectoryIfMissing True $ takeDirectory file
      copyFile (repoDir </> "examples" </> exampleDir </> file) file
    action

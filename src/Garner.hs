{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Garner
  ( Options (..),
    Env (..),
    run,
    runWith,
  )
where

import Control.Monad (void)
import qualified Data.Map as Map
import Development.Shake (Exit (Exit), cmd, cmd_)
import Garner.Common (nixArgs)
import Garner.Optparse
import Garner.Target
import Paths_garner
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (Handle, hPutStrLn, stderr)
import qualified System.IO
import qualified System.Posix.User as POSIX
import System.Process

data Env = Env
  { stdin :: Handle,
    tsRunnerFilename :: FilePath,
    userShell :: FilePath
  }

run :: IO ()
run = do
  env <- productionEnv
  targets <- readTargets (tsRunnerFilename env)
  opts' <- getOptions targets
  runWith env opts'

runWith :: Env -> Options -> IO ()
runWith env opts = case command opts of
  Run (RunOptions {..}) -> do
    void $ writeFlakeFile $ tsRunnerFilename env
    cmd_ "nix run" nixArgs (".#" <> target)
  Enter (EnterOptions {..}) -> do
    void $ writeFlakeFile $ tsRunnerFilename env
    let devProc =
          ( proc
              "nix"
              ("develop" : nixArgs <> [".#" <> target, "--command", userShell env])
          )
            { std_in = UseHandle $ stdin env,
              std_out = Inherit,
              std_err = Inherit
            }
    _ <- withCreateProcess devProc $ \_ _ _ procHandle -> do
      waitForProcess procHandle
    pure ()
  Start (StartOptions {..}) -> do
    config <- writeFlakeFile $ tsRunnerFilename env
    let maybeCommand = case Map.lookup target config of
          Nothing -> error $ "Could not find target " <> target
          Just targetConfig -> startCommand targetConfig
    let command = case maybeCommand of
          Nothing -> error "No start command configured"
          Just command -> command
    hPutStrLn stderr $ "[garner] Running \"" <> unwords command <> "\""
    Exit c <- cmd "nix develop" nixArgs (".#" <> target) "-c" command
    case c of
      ExitSuccess -> pure ()
      ExitFailure exitCode -> hPutStrLn stderr $ "[garner] \"" <> unwords command <> "\" exited with status code " <> show exitCode

productionEnv :: IO Env
productionEnv = do
  tsRunnerFilename <- getDataFileName "ts/runner.ts"
  userShell <- findUserShell
  pure $
    Env
      { stdin = System.IO.stdin,
        tsRunnerFilename,
        userShell
      }

findUserShell :: IO FilePath
findUserShell = do
  userId <- POSIX.getRealUserID
  POSIX.userShell <$> POSIX.getUserEntryForID userId

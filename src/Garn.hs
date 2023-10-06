{-# LANGUAGE DuplicateRecordFields #-}

module Garn
  ( Options (..),
    Env (..),
    run,
    readOptionsAndConfig,
    runWith,
  )
where

import Control.Monad (forM_, when)
import Development.Shake (Exit (Exit), cmd, cmd_)
import Garn.Common (nixArgs)
import Garn.GarnerConfig
import Garn.Init
import Garn.Optparse
import Paths_garn
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..), exitWith)
import System.IO (Handle, hPutStrLn, stderr)
import qualified System.IO
import qualified System.Posix.User as POSIX
import System.Process

data Env = Env
  { stdin :: Handle,
    tsRunnerFilename :: FilePath,
    initFileName :: FilePath,
    userShell :: FilePath
  }

run :: IO ()
run = do
  env <- productionEnv
  options <- readOptionsAndConfig env
  runWith env options

readOptionsAndConfig :: Env -> IO Options
readOptionsAndConfig env = do
  hasGarner <- doesFileExist "garner.ts"
  if hasGarner
    then do
      garnerConfig <- readGarnerConfig (tsRunnerFilename env)
      getOpts (WithGarnerTs garnerConfig)
    else getOpts WithoutGarnerTs

runWith :: Env -> Options -> IO ()
runWith env (WithoutGarnerTsOpts Init) = initGarnerTs $ initFileName env
runWith env (WithGarnerTsOpts garnerConfig opts) = do
  writeGarnerConfig garnerConfig
  case opts of
    Gen -> pure ()
    Run (CommandOptions {..}) -> do
      cmd_ "nix run" nixArgs (".#" <> target)
    Enter (CommandOptions {..}) -> do
      hPutStrLn stderr $ "[garner] Entering " <> target <> " shell. Type 'exit' to exit."
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
      hPutStrLn stderr $ "[garner] Exiting " <> target <> " shell."
      pure ()
    Check (CommandOptions {targetConfig}) -> do
      forM_ (checks targetConfig) $ \check -> do
        Exit c <- cmd "nix build" nixArgs (".#" <> check)
        when (c /= ExitSuccess) $ do
          exitWith c

productionEnv :: IO Env
productionEnv = do
  tsRunnerFilename <- getDataFileName "ts/runner.ts"
  initFileName <- getDataFileName "ts/init.ts"
  userShell <- findUserShell
  pure $
    Env
      { stdin = System.IO.stdin,
        tsRunnerFilename,
        initFileName,
        userShell
      }

findUserShell :: IO FilePath
findUserShell = do
  userId <- POSIX.getRealUserID
  POSIX.userShell <$> POSIX.getUserEntryForID userId

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
import Garn.Common (currentSystem, nixArgs)
import Garn.GarnConfig
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
  hasGarn <- doesFileExist "garn.ts"
  if hasGarn
    then do
      garnConfig <- readGarnConfig (tsRunnerFilename env)
      getOpts (WithGarnTs garnConfig)
    else getOpts WithoutGarnTs

runWith :: Env -> Options -> IO ()
runWith env (WithoutGarnTsOpts Init) = initGarnTs $ initFileName env
runWith env (WithGarnTsOpts garnConfig opts) = do
  writeGarnConfig garnConfig
  case opts of
    Gen -> pure ()
    Run (CommandOptions {..}) -> do
      cmd_ "nix run" nixArgs (".#" <> target)
    Enter (CommandOptions {..}) -> do
      hPutStrLn stderr $ "[garn] Entering " <> target <> " shell. Type 'exit' to exit."
      let devProc =
            ( proc
                "nix"
                ("develop" : nixArgs <> [".#" <> target, "--command", userShell env])
            )
              { std_in = UseHandle $ stdin env,
                std_out = Inherit,
                std_err = Inherit
              }
      c <- withCreateProcess devProc $ \_ _ _ procHandle -> do
        waitForProcess procHandle
      when (c /= ExitSuccess) $ exitWith c
      hPutStrLn stderr $ "[garn] Exiting " <> target <> " shell."
      pure ()
    Build (CommandOptions {targetConfig}) -> do
      forM_ (packages targetConfig) $ \package -> do
        Exit c <- cmd "nix build" nixArgs (".#" <> package)
        when (c /= ExitSuccess) $ exitWith c
    Check (CommandOptions {targetConfig}) -> do
      forM_ (packages targetConfig) $ \package -> do
        Exit c <- cmd "nix build" nixArgs (".#" <> package)
        when (c /= ExitSuccess) $ exitWith c
      system <- currentSystem
      forM_ (checks targetConfig) $ \check -> do
        Exit c <- cmd "nix build" nixArgs (".#checks." <> system <> "." <> check)
        when (c /= ExitSuccess) $ exitWith c

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

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
import Development.Shake (Exit (Exit), cmd)
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
    initFileName :: FilePath,
    userShell :: FilePath
  }

run :: IO ()
run = do
  env <- productionEnv
  options <- readOptionsAndConfig
  runWith env options

readOptionsAndConfig :: IO Options
readOptionsAndConfig = do
  hasGarn <- doesFileExist "garn.ts"
  if hasGarn
    then do
      garnConfig <- readGarnConfig
      getOpts (WithGarnTs garnConfig)
    else getOpts WithoutGarnTs

runWith :: Env -> Options -> IO ()
runWith env (WithoutGarnTsOpts Init) = initGarnTs $ initFileName env
runWith env (WithGarnTsOpts garnConfig opts) = do
  writeGarnConfig garnConfig
  case opts of
    Gen -> pure ()
    Run (CommandOptions {..}) argv -> do
      callProcess "nix" $ ["run"] <> nixArgs <> [".#" <> target, "--"] <> argv
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
  initFileName <- getDataFileName "ts/internal/init.ts"
  userShell <- findUserShell
  pure $
    Env
      { stdin = System.IO.stdin,
        initFileName,
        userShell
      }

findUserShell :: IO FilePath
findUserShell = do
  userId <- POSIX.getRealUserID
  POSIX.userShell <$> POSIX.getUserEntryForID userId

{-# LANGUAGE DuplicateRecordFields #-}

module Garn
  ( Env (..),
    Garn.run,
  )
where

import Control.Exception (catch)
import Control.Monad (forM_, when)
import Cradle
import Garn.Common (currentSystem, nixArgs)
import qualified Garn.Errors
import Garn.GarnConfig
import Garn.Init
import Garn.Optparse
import System.Directory (doesFileExist)
import System.Exit (exitWith)
import System.IO (Handle, hPutStrLn, stderr)

data Env = Env
  { stdin :: Handle,
    initFileName :: FilePath,
    userShell :: FilePath
  }

run :: Env -> IO ()
run env =
  (readOptionsAndConfig >>= runWith env)
    `catch` \(Garn.Errors.UserError err) -> do
      hPutStrLn stderr $ "[garn] Error: " <> err
      exitWith $ ExitFailure 1

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
      exitCode <- runNix (StdinHandle (stdin env), DelegateCtrlC, "run", ".#" <> asNixFacing target, "--", argv)
      exitWith exitCode
    Enter (CommandOptions {..}) -> do
      hPutStrLn stderr $
        "[garn] Entering "
          <> asUserFacing target
          <> " shell. Type 'exit' to exit."
      c <- runNix (StdinHandle (stdin env), "develop", ".#" <> asNixFacing target, "--command", userShell env)
      when (c /= ExitSuccess) $ exitWith c
      hPutStrLn stderr $ "[garn] Exiting " <> asUserFacing target <> " shell."
      pure ()
    Build (CommandOptions {targetConfig}) -> do
      case targetConfig of
        TargetConfigProject (ProjectTarget {packages}) -> do
          forM_ packages $ \package -> do
            c <- runNix (NoStdin, "build", ".#" <> package)
            when (c /= ExitSuccess) $ exitWith c
        TargetConfigExecutable _ -> pure ()
    Check checkOptions -> case checkOptions of
      (Qualified (CommandOptions {targetConfig})) -> do
        checkTarget targetConfig
      Unqualified -> do
        forM_ (targets garnConfig) checkTarget

checkTarget :: TargetConfig -> IO ()
checkTarget targetConfig = case targetConfig of
  TargetConfigProject (ProjectTarget {packages, checks}) -> do
    forM_ packages $ \package -> do
      c <- runNix (NoStdin, "build", ".#" <> package)
      when (c /= ExitSuccess) $ exitWith c
    system <- currentSystem
    forM_ checks $ \check -> do
      c <- runNix (NoStdin, "build", ".#checks." <> system <> "." <> check)
      when (c /= ExitSuccess) $ exitWith c
  TargetConfigExecutable _ -> pure ()

runNix :: (Input a, Output b) => a -> IO b
runNix = Cradle.run "nix" nixArgs

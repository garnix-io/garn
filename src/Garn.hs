{-# LANGUAGE DuplicateRecordFields #-}

module Garn
  ( Garn.run,
  )
where

import Control.Exception (catch)
import Control.Monad (forM_, when)
import Cradle
import Garn.Common (currentSystem, nixArgs)
import Garn.Env (Env (..))
import qualified Garn.Errors
import Garn.GarnConfig
import Garn.Init
import Garn.Optparse
import System.Directory (doesFileExist)
import System.Exit (exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn)

run :: Env -> IO ()
run env =
  (readOptionsAndConfig env >>= runWith env)
    `catch` \(Garn.Errors.UserError err) -> do
      hPutStrLn (stderr env) $ "[garn] Error: " <> err
      exitWith $ ExitFailure 1

readOptionsAndConfig :: Env -> IO Options
readOptionsAndConfig env = do
  hasGarn <- doesFileExist (workingDir env </> "garn.ts")
  if hasGarn
    then do
      garnConfig <- readGarnConfig env
      getOpts env (WithGarnTs garnConfig)
    else getOpts env WithoutGarnTs

runWith :: Env -> Options -> IO ()
runWith env (WithoutGarnTsOpts Init) = initGarnTs env $ initFileName env
runWith env (WithGarnTsOpts garnConfig opts) = do
  writeGarnConfig env garnConfig
  case opts of
    Gen -> pure ()
    Run (CommandOptions {..}) argv -> do
      exitCode <- runNix env (StdinHandle (stdin env), DelegateCtrlC, "run", ".#" <> asNixFacing target, "--", argv)
      exitWith exitCode
    Enter (CommandOptions {..}) -> do
      hPutStrLn (stderr env) $
        "[garn] Entering "
          <> asUserFacing target
          <> " shell. Type 'exit' to exit."
      c <- runNix env (StdinHandle (stdin env), "develop", ".#" <> asNixFacing target, "--command", userShell env)
      when (c /= ExitSuccess) $ exitWith c
      hPutStrLn (stderr env) $ "[garn] Exiting " <> asUserFacing target <> " shell."
      pure ()
    Build (CommandOptions {targetConfig, target}) -> do
      case targetConfig of
        TargetConfigProject (ProjectTarget {packages}) -> do
          forM_ packages $ \package -> do
            c <- runNix env (NoStdin, "build", ".#" <> package)
            when (c /= ExitSuccess) $ exitWith c
        TargetConfigPackage (PackageTarget {}) -> do
          c <- runNix env (NoStdin, "build", ".#" <> asNixFacing target)
          when (c /= ExitSuccess) $ exitWith c
        TargetConfigExecutable _ -> pure ()
    Check checkOptions -> case checkOptions of
      (Qualified (CommandOptions {targetConfig})) -> do
        checkTarget env targetConfig
      Unqualified -> do
        forM_ (targets garnConfig) (checkTarget env)

checkTarget :: Env -> TargetConfig -> IO ()
checkTarget env targetConfig = case targetConfig of
  TargetConfigProject (ProjectTarget {packages, checks}) -> do
    forM_ packages $ \package -> do
      c <- runNix env (NoStdin, "build", ".#" <> package)
      when (c /= ExitSuccess) $ exitWith c
    system <- currentSystem
    forM_ checks $ \check -> do
      c <- runNix env (NoStdin, "build", ".#checks." <> system <> "." <> check)
      when (c /= ExitSuccess) $ exitWith c
  TargetConfigPackage _ -> pure ()
  TargetConfigExecutable _ -> pure ()

runNix :: (Input a, Output b) => Env -> a -> IO b
runNix env =
  Cradle.run
    (WorkingDir (workingDir env))
    (StderrHandle (stderr env))
    "nix"
    nixArgs

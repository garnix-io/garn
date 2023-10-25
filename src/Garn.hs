{-# LANGUAGE DuplicateRecordFields #-}

module Garn
  ( Env (..),
    run,
  )
where

import Control.Exception (catch)
import Control.Monad (forM_, when)
import Development.Shake (Exit (Exit), cmd)
import Garn.Common (currentSystem, nixArgs)
import qualified Garn.Errors
import Garn.GarnConfig
import Garn.Init
import Garn.Optparse
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..), exitWith)
import System.IO (Handle, hPutStrLn, stderr)
import System.Process

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
      case targetConfig of
        TargetConfigProject (ProjectTarget {packages}) -> do
          forM_ packages $ \package -> do
            Exit c <- cmd "nix build" nixArgs [".#" <> package]
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
      Exit c <- cmd "nix build" nixArgs [".#" <> package]
      when (c /= ExitSuccess) $ exitWith c
    system <- currentSystem
    forM_ checks $ \check -> do
      Exit c <- cmd "nix build" nixArgs [".#checks." <> system <> "." <> check]
      when (c /= ExitSuccess) $ exitWith c
  TargetConfigExecutable _ -> pure ()

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Garner
  ( Options (..),
    Env (..),
    run,
    readOptionsAndConfig,
    runWith,
  )
where

import Development.Shake (Exit (Exit), cmd, cmd_)
import Garner.Common (nixArgs)
import Garner.Optparse
import Garner.Target
import Paths_garner
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
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
  (options, garnerConfig) <- readOptionsAndConfig env
  runWith env options garnerConfig

readOptionsAndConfig :: Env -> IO (Options, GarnerConfig)
readOptionsAndConfig env = do
  garnerConfig <- readGarnerConfig (tsRunnerFilename env)
  options <- getOptions $ targets garnerConfig
  pure (options, garnerConfig)

runWith :: Env -> Options -> GarnerConfig -> IO ()
runWith env opts garnerConfig = do
  writeGarnerConfig garnerConfig
  case command opts of
    Gen -> pure ()
    Run (CommandOptions {..}) -> do
      cmd_ "nix run" nixArgs (".#" <> target)
    Enter (CommandOptions {..}) -> do
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
    Start (CommandOptions {..}) -> do
      let command = case startCommand targetConfig of
            Nothing -> error "No start command configured"
            Just command -> command
      hPutStrLn stderr $ "[garner] Running \"" <> unwords command <> "\""
      Exit c <- cmd "nix develop" nixArgs (".#" <> target) "-c" command
      case c of
        ExitSuccess -> pure ()
        ExitFailure exitCode -> hPutStrLn stderr $ "[garner] \"" <> unwords command <> "\" exited with status code " <> show exitCode
    Ci (CommandOptions {target}) -> do
      Exit c <- cmd "nix build" nixArgs (".#" <> target)
      exitWith c

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

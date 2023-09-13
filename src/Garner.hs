{-# LANGUAGE QuasiQuotes #-}

module Garner
  ( Options (..),
    run,
    runWith,
  )
where

import Control.Monad (void)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import Development.Shake (CmdOption (..), Exit (Exit), Stdout (Stdout), cmd, cmd_)
import Garner.Common (nixpkgsInput)
import Paths_garner
import System.Directory
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (Handle, hClose, hPutStr, hPutStrLn, stderr)
import qualified System.IO
import System.IO.Temp
import qualified System.Posix.User as POSIX
import System.Process
import WithCli

data Options = Options
  { stdin :: Handle,
    tsRunnerFilename :: FilePath,
    userShell :: FilePath
  }

run :: IO ()
run = runWith =<< productionOptions

runWith :: Options -> IO ()
runWith opts = withCli $ \(command :: String) (target :: String) -> case command of
  "run" -> do
    void $ makeFlake opts
    cmd_ "nix run" nixArgs (".#" <> target)
  "enter" -> do
    void $ makeFlake opts
    let devProc =
          ( proc
              "nix"
              ("develop" : nixArgs <> [".#" <> target, "--command", userShell opts])
          )
            { std_in = UseHandle $ stdin opts,
              std_out = Inherit,
              std_err = Inherit
            }
    _ <- withCreateProcess devProc $ \_ _ _ procHandle -> do
      waitForProcess procHandle
    pure ()
  "start" -> do
    config <- makeFlake opts
    let maybeConfig = case Map.lookup target config of
          Nothing -> error $ "Could not find target " <> target
          Just targetConfig -> targetConfig
    case setupCommand maybeConfig of
          Nothing -> pure ()
          Just setupCommand -> do
            Exit c <- cmd "nix develop" nixArgs (".#" <> target) "-c" setupCommand
            case c of
              ExitSuccess -> pure ()
              ExitFailure exitCode -> hPutStrLn stderr $ "[garner] \"" <> unwords setupCommand <> "\" exited with status code " <> show exitCode
    let command = case startCommand maybeConfig of
          Nothing -> error "No start command configured"
          Just command -> command
    hPutStrLn stderr $ "[garner] Running \"" <> unwords command <> "\""
    Exit c <- cmd "nix develop" nixArgs (".#" <> target) "-c" command
    case c of
      ExitSuccess -> pure ()
      ExitFailure exitCode -> do
        error $ "[garner] \"" <> unwords command <> "\" exited with status code " <> show exitCode
  _ -> error $ "Command " <> command <> " not supported."

data TargetConfig = TargetConfig
  { startCommand :: Maybe [String],
    setupCommand :: Maybe [String]
  }
  deriving (Generic, FromJSON)

makeFlake :: Options -> IO (Map String TargetConfig)
makeFlake opts = do
  dir <- getCurrentDirectory
  withSystemTempFile "garner-main.ts" $ \mainPath mainHandle -> do
    hPutStr
      mainHandle
      [i|
        import * as config from "#{dir}/garner.ts"
        import { writeFlake } from "#{tsRunnerFilename opts}"

        writeFlake("#{nixpkgsInput}", config)
      |]
    hClose mainHandle
    Stdout out <- cmd "deno run --quiet --check --allow-write" mainPath
    cmd_ [EchoStderr False, EchoStdout False] "nix" nixArgs "fmt ./flake.nix"
    case eitherDecode out of
      Left err -> error $ "Unexpected package export from garner.ts: " <> err
      Right targetConfigMap -> return targetConfigMap

productionOptions :: IO Options
productionOptions = do
  tsRunnerFilename <- getDataFileName "ts/runner.ts"
  userShell <- findUserShell
  pure $
    Options
      { stdin = System.IO.stdin,
        tsRunnerFilename,
        userShell
      }

findUserShell :: IO FilePath
findUserShell = do
  userId <- POSIX.getRealUserID
  POSIX.userShell <$> POSIX.getUserEntryForID userId

nixArgs :: [String]
nixArgs =
  [ "--extra-experimental-features",
    "flakes nix-command",
    "--print-build-logs"
  ]

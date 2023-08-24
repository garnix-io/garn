{-# LANGUAGE QuasiQuotes #-}

module Garner
  ( Options (..),
    run,
    runWith,
  )
where

import Data.String.Interpolate (i)
import Development.Shake (CmdOption (..), cmd_)
import Paths_garner
import System.Directory
import System.IO (Handle, hClose, hPutStr)
import qualified System.IO
import System.IO.Temp
import qualified System.Posix.User as POSIX
import System.Process
import WithCli

data Options = Options
  { stdin :: Handle,
    tsRunnerFilename :: String,
    findUserShell :: IO String
  }

run :: IO ()
run = do
  tsRunnerFilename <- getDataFileName "ts/runner.ts"
  runWith
    Options
      { stdin = System.IO.stdin,
        tsRunnerFilename,
        findUserShell = fmap POSIX.userShell . POSIX.getUserEntryForID =<< POSIX.getRealUserID
      }

runWith :: Options -> IO ()
runWith opts = withCli $ \(command :: String) (target :: String) -> case command of
  "run" -> do
    makeFlake opts
    cmd_ "nix run" nixArgs (".#" <> target)
  "enter" -> do
    makeFlake opts
    shell <- findUserShell opts
    let devProc =
          ( proc
              "nix"
              ("develop" : nixArgs <> [".#" <> target, "--command", shell])
          )
            { std_in = UseHandle $ stdin opts,
              std_out = Inherit,
              std_err = Inherit
            }
    _ <- withCreateProcess devProc $ \_ _ _ procHandle -> do
      waitForProcess procHandle
    pure ()
  _ -> error $ "Command " <> command <> " not supported."

makeFlake :: Options -> IO ()
makeFlake opts = do
  dir <- getCurrentDirectory
  withSystemTempFile "garner-main.ts" $ \mainPath mainHandle -> do
    hPutStr
      mainHandle
      [i|
        import * as config from "#{dir}/garner.ts"
        import { writeFlake } from "#{tsRunnerFilename opts}"

        writeFlake(config)
      |]
    hClose mainHandle
    cmd_ "deno run --quiet --check --allow-write" mainPath
    cmd_ [EchoStderr False, EchoStdout False] "nix" nixArgs "fmt ./flake.nix"

nixArgs :: [String]
nixArgs =
  [ "--extra-experimental-features",
    "flakes nix-command",
    "--print-build-logs"
  ]

{-# LANGUAGE QuasiQuotes #-}

module Garner where

import Data.String.Interpolate (i)
import Development.Shake (cmd_)
import Paths_garner
import System.Directory
import System.IO (Handle, hClose, hPutStr)
import qualified System.IO
import System.IO.Temp
import System.Process
import WithCli

data Options = Options
  { stdin :: Handle,
    tsRunnerFilename :: String
  }

run :: IO ()
run = do
  tsRunnerFilename <- getDataFileName "ts/runner.ts"
  runWith
    Options
      { stdin = System.IO.stdin,
        tsRunnerFilename
      }

runWith :: Options -> IO ()
runWith opts = withCli $ \(command :: String) (target :: String) -> case command of
  "run" -> do
    makeFlake opts
    cmd_ "nix run" nixArgs (".#" <> target)
  "enter" -> do
    makeFlake opts
    let devProc =
          ( proc
              "nix"
              ["develop", "-L", ".#" <> target, "-c", "bash"]
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

nixArgs :: [String]
nixArgs = ["-L", "--extra-experimental-features", "nix-command flakes"]

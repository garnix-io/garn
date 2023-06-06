{-# LANGUAGE QuasiQuotes #-}

module Garner where

import Control.Monad
import Data.String.Interpolate (i)
import Development.Shake (cmd_)
import Paths_garner
import System.IO (Handle)
import qualified System.IO
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
              ["develop", "-L", ".#" <> target]
          )
            { std_in = UseHandle $ stdin opts,
              std_out = Inherit,
              std_err = Inherit
            }
    (_, _, _, procHandle) <- createProcess devProc
    void $ waitForProcess procHandle
  _ -> error $ "Command " <> command <> " not supported."

makeFlake :: Options -> IO ()
makeFlake opts = do
  writeFile
    "main.ts"
    [i|
        import * as config from "./garner.ts"
        import { writeFlake } from "#{tsRunnerFilename opts}"

        writeFlake(config)
      |]
  cmd_ "deno run --check --allow-write main.ts"

nixArgs :: [String]
nixArgs = ["-L", "--extra-experimental-features", "nix-command flakes"]

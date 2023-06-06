{-# LANGUAGE QuasiQuotes #-}

module Garner where

import Data.String.Interpolate (i)
import Development.Shake
import System.IO (hPutStrLn, stderr)
import WithCli

data Options = Options
  {tsRunnerFilename :: String}

run :: IO ()
run = runWith Options {tsRunnerFilename = "todo"}

runWith :: Options -> IO ()
runWith opts = withCli $ \(command :: String) (target :: String) -> case command of
  "run" -> do
    writeFile
      "main.ts"
      [i|
        import * as config from "./garner.ts"
        import { writeFlake } from "#{tsRunnerFilename opts}"

        writeFlake(config)
      |]
    cmd_ "deno run --check --allow-write main.ts"
    cmd_ "nix run" nixArgs (".#" <> target)
  _ -> error $ "Command " <> command <> " not supported."

nixArgs :: [String]
nixArgs = ["-L", "--extra-experimental-features", "nix-command flakes"]

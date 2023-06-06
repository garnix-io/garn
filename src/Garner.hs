{-# LANGUAGE QuasiQuotes #-}

module Garner where

import Data.String.Interpolate (i)
import Development.Shake
import WithCli

data Options = Options
  { tsRunnerFilename :: String }

run :: IO ()
run = runWith Options { tsRunnerFilename = "todo" }

runWith :: Options -> IO ()
runWith opts = withCli $ \(cmd :: String) (target :: String) -> case cmd of
  "run" -> do
    writeFile
      "main.ts"
      [i|
        import * as config from "./garner.ts"
        import { writeFlake } from "#{tsRunnerFilename opts}"

        writeFlake(config)
      |]
    cmd_ ("deno run --check --allow-write main.ts" :: String) :: IO ()
    cmd_ ("nix run -L .#" <> target) :: IO ()
  _ -> error $ "Command " <> cmd <> " not supported."

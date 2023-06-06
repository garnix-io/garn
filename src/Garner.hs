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
runWith opts = withCli $ \("run" :: String) ("foo" :: String) -> do
  writeFile
    "main.ts"
    [i|
      import * as config from "./garner.ts"
      import { writeFlake } from "#{tsRunnerFilename opts}"

      writeFlake(config)
    |]
  cmd_ ("deno run --allow-write main.ts" :: String) :: IO ()
  cmd_ ("nix run -L .#foo" :: String) :: IO ()

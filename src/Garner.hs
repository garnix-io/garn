{-# LANGUAGE QuasiQuotes #-}

module Garner where

import Data.String.Interpolate (i)
import Development.Shake
import WithCli

run :: IO ()
run = withCli $ \("run" :: String) ("foo" :: String) -> do
  writeFile
    "main.ts"
    [i|
      import "./garner.ts"
    |]
  cmd_ ("deno run main.ts" :: String) :: IO ()

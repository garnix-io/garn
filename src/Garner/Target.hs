{-# LANGUAGE QuasiQuotes #-}

module Garner.Target where

import Control.Monad
import Data.Aeson (FromJSON, eitherDecode)
import Data.Map (Map)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util
import Development.Shake (CmdOption (EchoStderr, EchoStdout), Stdout (Stdout), cmd, cmd_)
import GHC.Generics (Generic)
import Garner.Common (nixArgs, nixpkgsInput)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hClose, hPutStr, stderr)
import System.IO.Temp (withSystemTempFile)

type Targets = Map String TargetConfig

data TargetConfig = TargetConfig
  { startCommand :: Maybe [String],
    description :: String
  }
  deriving (Generic, FromJSON)

writeFlakeFile :: String -> IO Targets
readTargets :: String -> IO Targets
(writeFlakeFile, readTargets) = (flakeRunner True, flakeRunner False)
  where
    flakeRunner :: Bool -> String -> IO Targets
    flakeRunner writeFlakeFile tsRunner = do
      checkGarnerFileExists
      dir <- getCurrentDirectory
      let makeString =
            if writeFlakeFile
              then "writeFlake(\"" <> nixpkgsInput <> "\", config)"
              else ""
      withSystemTempFile "garner-main.ts" $ \mainPath mainHandle -> do
        hPutStr
          mainHandle
          [i|
            import * as config from "#{dir}/garner.ts"
            import { writeFlake } from "#{tsRunner}"

            #{makeString}
            console.log(JSON.stringify(config));
          |]
        hClose mainHandle
        Stdout out <- cmd "deno run --quiet --check --allow-write" mainPath
        when writeFlakeFile $
          cmd_ [EchoStderr False, EchoStdout False] "nix" nixArgs "fmt ./flake.nix"
        case eitherDecode out of
          Left err -> error $ "Unexpected package export from garner.ts: " <> err
          Right targetConfigMap -> return targetConfigMap

checkGarnerFileExists :: IO ()
checkGarnerFileExists = do
  exists <- doesFileExist "garner.ts"
  when (not exists) $ do
    hPutStr stderr $
      unindent
        [i|
          No `garner.ts` file found in the current directory.

          Here's an example `garner.ts` file for npm frontends:

            import { mkNpmFrontend } from "http://localhost:8777/typescript.ts";

            export const frontend = mkNpmFrontend({
              src: "./.",
              description: "An NPM frontend",
            });

        |]
    exitWith $ ExitFailure 1

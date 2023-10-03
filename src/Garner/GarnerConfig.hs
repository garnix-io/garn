{-# LANGUAGE QuasiQuotes #-}

module Garner.GarnerConfig where

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

data GarnerConfig = GarnerConfig
  { targets :: Targets,
    flakeFile :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

type Targets = Map String TargetConfig

data TargetConfig = TargetConfig
  { startCommand :: Maybe [String],
    description :: String
  }
  deriving (Generic, FromJSON, Eq, Show)

readGarnerConfig :: String -> IO GarnerConfig
readGarnerConfig tsRunner = do
  checkGarnerFileExists
  dir <- getCurrentDirectory
  withSystemTempFile "garner-main.ts" $ \mainPath mainHandle -> do
    hPutStr
      mainHandle
      [i|
        import * as targets from "#{dir}/garner.ts"
        import { formatFlake } from "#{tsRunner}"

        console.log(JSON.stringify({
          targets,
          flakeFile: formatFlake("#{nixpkgsInput}", targets),
        }));
      |]
    hClose mainHandle
    Stdout out <- cmd "deno run --quiet --check --allow-write" mainPath
    case eitherDecode out :: Either String GarnerConfig of
      Left err -> error $ "Unexpected package export from garner.ts:\n" <> err
      Right writtenConfig -> return writtenConfig

writeGarnerConfig :: GarnerConfig -> IO ()
writeGarnerConfig garnerConfig = do
  writeFile "flake.nix" $ flakeFile garnerConfig
  cmd_ [EchoStderr False, EchoStdout False] "nix" nixArgs "fmt ./flake.nix"

checkGarnerFileExists :: IO ()
checkGarnerFileExists = do
  exists <- doesFileExist "garner.ts"
  when (not exists) $ do
    hPutStr stderr $
      unindent
        [i|
          No `garner.ts` file found in the current directory.

          Here's an example `garner.ts` file for npm frontends:

            import * as garner from "http://localhost:8777/mod.ts";

            export const frontend = garner.typescript.mkNpmFrontend({
              src: "./.",
              description: "An NPM frontend",
            });

        |]
    exitWith $ ExitFailure 1

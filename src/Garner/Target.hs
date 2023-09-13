{-# LANGUAGE QuasiQuotes #-}

module Garner.Target where

import Control.Monad
import Data.Aeson (FromJSON, eitherDecode)
import Data.Map (Map)
import Data.String.Interpolate (i)
import Development.Shake (CmdOption (EchoStderr, EchoStdout), Stdout (Stdout), cmd, cmd_)
import GHC.Generics (Generic)
import Garner.Common (nixArgs, nixpkgsInput)
import System.Directory (getCurrentDirectory)
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)

data TargetConfig = TargetConfig
  { startCommand :: Maybe [String],
    description :: String
  }
  deriving (Generic, FromJSON)

makeFlake :: String -> IO Targets
getFlake :: String -> IO Targets
(makeFlake, getFlake) = (flakeRunner True, flakeRunner False)
  where
    flakeRunner :: Bool -> String -> IO Targets
    flakeRunner alsoMake tsRunner = do
      dir <- getCurrentDirectory
      let makeString =
            if alsoMake
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
        when alsoMake $
          cmd_ [EchoStderr False, EchoStdout False] "nix" nixArgs "fmt ./flake.nix"
        case eitherDecode out of
          Left err -> error $ "Unexpected package export from garner.ts: " <> err
          Right targetConfigMap -> return targetConfigMap

type Targets = Map String TargetConfig
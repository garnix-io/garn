{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Garn.GarnConfig where

import Control.Exception (throwIO)
import Control.Monad
import Cradle (Stderr (..), StderrHandle (..), StdoutUntrimmed (..), run)
import Data.Aeson
  ( FromJSON (parseJSON),
    FromJSONKey,
    Value (Object),
    defaultOptions,
    eitherDecode,
    genericParseJSON,
    withObject,
    (.:),
  )
import Data.List (intercalate)
import Data.Map (Map)
import Data.String (IsString (fromString))
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import GHC.Generics (Generic)
import Garn.Common (garnCliVersion, nixArgs, nixpkgsInput)
import Garn.Env (Env (..))
import qualified Garn.Errors
import System.Directory (makeAbsolute)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)

-- This needs to be in sync with `DenoOutput` in runner.ts
data DenoOutput
  = Success Version GarnConfig
  | UserError Version String
  deriving stock (Eq, Show, Generic)

instance FromJSON DenoOutput where
  parseJSON = withObject "DenoOutput" $ \o -> do
    garnTsLibVersion <- o .: fromString "garnTsLibVersion"
    tag <- o .: fromString "tag"
    contents <- o .: fromString "contents"
    case tag of
      "Success" ->
        Success garnTsLibVersion <$> genericParseJSON defaultOptions contents
      "UserError" ->
        UserError garnTsLibVersion <$> parseJSON contents
      _ -> fail $ "Unknown DenoOutput tag: " <> tag

newtype Version = Version String
  deriving newtype (Eq, Show, FromJSON)

data GarnConfig = GarnConfig
  { targets :: Targets,
    flakeFile :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

newtype TargetName = TargetName {asNixFacing :: String}
  deriving stock (Generic, Show, Eq, Ord)
  deriving newtype (FromJSONKey)

fromUserFacing :: String -> TargetName
fromUserFacing = TargetName . fmap sully
  where
    sully '.' = '/'
    sully x = x

asUserFacing :: TargetName -> String
asUserFacing (TargetName name) = clean <$> name
  where
    clean '/' = '.'
    clean x = x

type Targets = Map TargetName TargetConfig

data TargetConfig
  = TargetConfigProject ProjectTarget
  | TargetConfigPackage PackageTarget
  | TargetConfigExecutable ExecutableTarget
  deriving (Generic, Eq, Show)

data ProjectTarget = ProjectTarget
  { description :: String,
    packages :: [String],
    checks :: [String],
    runnable :: Bool
  }
  deriving (Generic, FromJSON, Eq, Show)

data PackageTarget = PackageTarget
  { description :: String
  }
  deriving (Generic, FromJSON, Eq, Show)

data ExecutableTarget = ExecutableTarget
  { description :: String
  }
  deriving (Generic, Eq, Show, FromJSON)

instance FromJSON TargetConfig where
  parseJSON = withObject "TargetConfig" $ \o -> do
    tag <- o .: fromString "tag"
    case tag of
      "project" -> TargetConfigProject <$> genericParseJSON defaultOptions (Object o)
      "package" -> TargetConfigPackage <$> genericParseJSON defaultOptions (Object o)
      "executable" -> TargetConfigExecutable <$> genericParseJSON defaultOptions (Object o)
      _ -> fail $ "Unknown target tag: " <> tag

getDescription :: TargetConfig -> String
getDescription = \case
  TargetConfigProject (ProjectTarget {description}) -> description
  TargetConfigPackage (PackageTarget {description}) -> description
  TargetConfigExecutable (ExecutableTarget {description}) -> description

readGarnConfig :: Env -> IO GarnConfig
readGarnConfig env = do
  withSystemTempFile "garn-main.js" $ \mainPath mainHandle -> do
    dir <- makeAbsolute (workingDir env)
    hPutStr
      mainHandle
      [i|
        import * as garnExports from "#{dir}/garn.ts"

        if (window.__garnGetInternalLib == null) {
          console.log("null");
        } else {
          const internalLib = window.__garnGetInternalLib();
          const { toDenoOutput } = internalLib;
          console.log(JSON.stringify(toDenoOutput("#{nixpkgsInput}", garnExports)));
        }
      |]
    hClose mainHandle
    (exitCode, StdoutUntrimmed (cs -> out)) <-
      run
        (StderrHandle (stderr env))
        (words "deno run --quiet --check --allow-write --allow-run --allow-read")
        mainPath
    when (exitCode /= ExitSuccess) $ do
      exitWith exitCode
    case eitherDecode out :: Either String (Maybe DenoOutput) of
      Left err -> do
        let suggestion = lines $ case eitherDecode out :: Either String OnlyTsLibVersion of
              Left err ->
                [i|Try updating the garn typescript library! (#{err})|]
              Right (OnlyTsLibVersion {garnTsLibVersion}) ->
                unindent
                  [i|
                    garn cli tool version: #{garnCliVersion}
                    garn typescript library version: #{garnTsLibVersion}

                    Either:

                      Install version #{garnTsLibVersion} of the garn cli tool.
                      See https://garn.io/docs/getting_started#updating-garn for how to update.

                    Or:

                      Use version #{garnCliVersion} of the typescript library.
                      E.g.: import * as garn from "https://garn.io/ts/#{garnCliVersion}/mod.ts";

                  |]
        throwIO $
          Garn.Errors.UserError $
            intercalate
              "\n"
              ( "Version mismatch detected:"
                  : map
                    indent
                    ( suggestion
                        <> ["(Internal details: " <> err <> ")"]
                    )
              )
      Right Nothing -> error $ "No garn library imported in garn.ts"
      Right (Just (UserError _tsLibVersion err)) -> throwIO $ Garn.Errors.UserError err
      Right (Just (Success _tsLibVersion writtenConfig)) -> return writtenConfig

indent :: String -> String
indent = \case
  "" -> ""
  line -> "  " <> line

data OnlyTsLibVersion = OnlyTsLibVersion
  { garnTsLibVersion :: Version
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

writeGarnConfig :: Env -> GarnConfig -> IO ()
writeGarnConfig env garnConfig = do
  writeFile (workingDir env </> "flake.nix") $ flakeFile garnConfig
  (StdoutUntrimmed _, Stderr _) <- run "nix" nixArgs "run" (nixpkgsInput <> "#nixpkgs-fmt") "./flake.nix"
  pure ()

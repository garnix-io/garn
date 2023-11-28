{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Garn.GarnConfig where

import Control.Monad
import Cradle (Stderr (..), StdoutUntrimmed (..), run)
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
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hClose, hPutStr, stderr)
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
  | TargetConfigEnvironment EnvironmentTarget
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

data EnvironmentTarget = EnvironmentTarget
  { description :: Maybe String
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
      "environment" -> TargetConfigEnvironment <$> genericParseJSON defaultOptions (Object o)
      "package" -> TargetConfigPackage <$> genericParseJSON defaultOptions (Object o)
      "executable" -> TargetConfigExecutable <$> genericParseJSON defaultOptions (Object o)
      _ -> fail $ "Unknown target tag: " <> tag

getDescription :: TargetConfig -> Maybe String
getDescription = \case
  TargetConfigProject (ProjectTarget {description}) -> Just description
  TargetConfigEnvironment (EnvironmentTarget {description}) -> description
  TargetConfigPackage (PackageTarget {description}) -> Just description
  TargetConfigExecutable (ExecutableTarget {description}) -> Just description

newtype ReadConfigError = ReadConfigError {errorMessage :: String}
  deriving stock (Eq, Show)

readGarnConfig :: IO (Either ReadConfigError GarnConfig)
readGarnConfig = do
  checkGarnFileExists
  dir <- getCurrentDirectory
  withSystemTempFile "garn-main.js" $ \mainPath mainHandle -> do
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
    (exitCode, Stderr (cs -> err), StdoutUntrimmed (cs -> out)) <-
      run
        (words "deno run --quiet --check --allow-write --allow-run --allow-read")
        mainPath
    case exitCode of
      ExitFailure _ -> return . Left $ ReadConfigError $ "Running garn.ts failed:\n" <> err
      ExitSuccess -> case eitherDecode out :: Either String (Maybe DenoOutput) of
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
          return . Left $
            ReadConfigError $
              intercalate
                "\n"
                ( "Version mismatch detected:"
                    : map
                      indent
                      ( suggestion
                          <> ["(Internal details: " <> err <> ")"]
                      )
                )
        Right Nothing -> return $ Left $ ReadConfigError "No garn library imported in garn.ts"
        Right (Just (UserError _tsLibVersion err)) -> return . Left $ ReadConfigError err
        Right (Just (Success _tsLibVersion writtenConfig)) -> return $ Right writtenConfig

indent :: String -> String
indent = \case
  "" -> ""
  line -> "  " <> line

data OnlyTsLibVersion = OnlyTsLibVersion
  { garnTsLibVersion :: Version
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

writeGarnConfig :: GarnConfig -> IO ()
writeGarnConfig garnConfig = do
  writeFile "flake.nix" $ flakeFile garnConfig
  (StdoutUntrimmed _, Stderr _) <- run "nix" nixArgs "run" (nixpkgsInput <> "#nixpkgs-fmt") "./flake.nix"
  pure ()

checkGarnFileExists :: IO ()
checkGarnFileExists = do
  exists <- doesFileExist "garn.ts"
  when (not exists) $ do
    hPutStr stderr $
      unindent
        [i|
          No `garn.ts` file found in the current directory.

          Here's an example `garn.ts` file for npm frontends:

            import * as garn from "http://localhost:8777/mod.ts";

            export const frontend = garn.typescript.mkNpmProject({
              src: "./.",
              description: "An NPM frontend",
            });

        |]
    exitWith $ ExitFailure 1

{-# LANGUAGE DuplicateRecordFields #-}

module Garner.Optparse
  ( getOptions,
    optionsParser,
    Options (..),
    Command (..),
    CommandOptions (..),
  )
where

import qualified Data.Map as Map
import Garner.GarnerConfig
import Options.Applicative hiding (command)
import qualified Options.Applicative as OA

getOptions :: Maybe Targets -> IO Options
getOptions mtargets = do
  let o = case mtargets of
        Just targets -> opts (optionsParser targets)
        Nothing -> Init <$> opts initParser
  customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty) o
  where
    opts parser =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Develop, build, and test your projects reliably and easily"
            <> header "garner - the project manager"
        )

data Options
  = Options {command :: Command}
  | Init InitOptions
  deriving stock (Eq, Show)

optionsParser :: Targets -> Parser Options
optionsParser targets =
  Options <$> commandParser targets

data Command
  = Gen
  | Run CommandOptions
  | Enter CommandOptions
  | Start CommandOptions
  | Ci CommandOptions
  deriving stock (Eq, Show)

commandParser :: Targets -> Parser Command
commandParser targets =
  subparser $
    mconcat
      [ OA.command "ci" (info (withCommandOptions Ci) (progDesc "Run the garnix ci tests locally")),
        OA.command "gen" (info (pure Gen) (progDesc "Generate the flake.nix file and exit")),
        OA.command "run" (info (withCommandOptions Run) (progDesc "Build and run the default executable of a target")),
        OA.command "enter" (info (withCommandOptions Enter) (progDesc "Enter a devshell for a target")),
        OA.command "start" (info (withCommandOptions Start) (progDesc "Start the startCommand process of a target"))
      ]
  where
    withCommandOptions constructor =
      constructor <$> commandOptionsParser targets

initParser :: Parser InitOptions
initParser =
  subparser $
    OA.command "init" $
      info (pure InitOptions) $
        progDesc "Infer a garner.ts file from the project layout"

data CommandOptions = CommandOptions
  { target :: String,
    targetConfig :: TargetConfig
  }
  deriving stock (Eq, Show)

commandOptionsParser :: Targets -> Parser CommandOptions
commandOptionsParser targets =
  ( subparser
      $ foldMap
        ( \(target, targetConfig) ->
            OA.command
              target
              ( info
                  (pure (CommandOptions target targetConfig))
                  (progDesc $ description targetConfig)
              )
        )
      $ Map.assocs targets
  )
    <**> helper

data InitOptions = InitOptions
  deriving stock (Eq, Show)

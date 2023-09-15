{-# LANGUAGE DuplicateRecordFields #-}

module Garner.Optparse
  ( getWithGarnerTsOptions,
    getWithoutGarnerTsOptions,
    Options (..),
    WithGarnerTsCommand (..),
    WithoutGarnerTsCommand (..),
    -- Command (..),
    CommandOptions (..),
  )
where

import qualified Data.Map as Map
import Garner.GarnerConfig
import Options.Applicative hiding (command)
import qualified Options.Applicative as OA

getWithGarnerTsOptions :: Targets -> IO WithGarnerTsCommand
getWithGarnerTsOptions = mkOpts . withGarnerTsParser

getWithoutGarnerTsOptions :: IO WithoutGarnerTsCommand
getWithoutGarnerTsOptions = mkOpts withoutGarnerTsParser

mkOpts :: Parser a -> IO a
mkOpts parser =
  customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty) $ opts
  where
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Develop, build, and test your projects reliably and easily"
            <> header "garner - the project manager"
        )

data Options
  = WithGarnerTsOpts WithGarnerTsCommand GarnerConfig
  | WithoutGarnerTsOpts WithoutGarnerTsCommand

{-
optionsParser :: Targets -> Parser Options
optionsParser targets =
  Options <$> commandParser targets
  -}

data WithoutGarnerTsCommand
  = Init

data WithGarnerTsCommand
  = Gen
  | Run CommandOptions
  | Enter CommandOptions
  | Start CommandOptions
  | Ci CommandOptions
  deriving stock (Eq, Show)

withGarnerTsParser :: Targets -> Parser WithGarnerTsCommand
withGarnerTsParser targets =
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

withoutGarnerTsParser :: Parser WithoutGarnerTsCommand
withoutGarnerTsParser =
  subparser $
    OA.command "init" $
      info (pure Init) $
        progDesc "Infer a garner.ts file from the project layout"

data CommandOptions = CommandOptions
  { target :: String,
    targetConfig :: TargetConfig
  }
  deriving stock (Eq, Show)

commandOptionsParser :: Targets -> Parser CommandOptions
commandOptionsParser targets =
  subparser
    ( foldMap
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

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
import Garner.Target
import Options.Applicative hiding (command)
import qualified Options.Applicative as OA

getOptions :: Targets -> IO Options
getOptions targets = do
  customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty) opts
  where
    opts =
      info
        (optionsParser targets <**> helper)
        ( fullDesc
            <> progDesc "Develop, build, and test your projects reliably and easily"
            <> header "garner - the project manager"
        )

data Options = Options
  {command :: Command}
  deriving stock (Eq, Show)

optionsParser :: Targets -> Parser Options
optionsParser targets =
  Options <$> commandParser targets

data CommandOptions = CommandOptions
  { target :: String
  }
  deriving stock (Eq, Show)

commandOptionsParser :: Targets -> Parser CommandOptions
commandOptionsParser targets =
  CommandOptions
    <$> ( subparser
            $ foldMap
              ( \(target, targetOpts) ->
                  OA.command target (info (pure target) (progDesc $ description targetOpts))
              )
            $ Map.assocs targets
        )
    <**> helper

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

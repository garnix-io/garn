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
getOptions targets = customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty) opts
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

data CommandOptions = CommandOptions
  { target :: String
  }
  deriving stock (Eq, Show)

data Command
  = Run CommandOptions
  | Enter CommandOptions
  | Start CommandOptions
  deriving stock (Eq, Show)

optionsParser :: Targets -> Parser Options
optionsParser targets =
  Options
    <$> commandParser targets

commandParser :: Targets -> Parser Command
commandParser targets =
  subparser
    ( OA.command "run" (info runCmd (progDesc "Build and run the default executable of a target"))
        <> OA.command "enter" (info enterCmd (progDesc "Enter a devshell for a target"))
        <> OA.command "start" (info startCmd (progDesc "Start the startCommand process of a target"))
    )
  where
    runCmd = Run . CommandOptions <$> targetParser <**> helper
    enterCmd = Enter . CommandOptions <$> targetParser <**> helper
    startCmd = Start . CommandOptions <$> targetParser <**> helper
    targetParser :: Parser String
    targetParser =
      subparser
        $ foldMap
          ( \(target, targetOpts) ->
              OA.command target (info (pure target) (progDesc $ description targetOpts))
          )
        $ Map.assocs targets

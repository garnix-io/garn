{-# LANGUAGE DuplicateRecordFields #-}

module Garner.Optparse
  ( getOpts,
    Options (..),
    OptionType (..),
    WithGarnerTsCommand (..),
    WithoutGarnerTsCommand (..),
    CommandOptions (..),
  )
where

import qualified Data.Map as Map
import Garner.GarnerConfig
import Options.Applicative hiding (command)
import qualified Options.Applicative as OA
import qualified Options.Applicative.Help.Pretty as OA
import qualified Text.PrettyPrint.ANSI.Leijen as PP

getOpts :: OptionType -> IO Options
getOpts oType =
  customExecParser (prefs $ showHelpOnError <> showHelpOnEmpty) opts
  where
    unavailable :: OA.Doc
    unavailable =
      let formatCommands cmdInfo = [PP.string cmd | (cmd, _, _) <- cmdInfo]
       in PP.nest
            2
            $ PP.vsep
              ( PP.string "Unavailable commands:"
                  : case oType of
                    WithGarnerTs _ -> formatCommands withouGarnerTsCommandInfo
                    WithoutGarnerTs -> formatCommands withGarnerTsCommandInfo
              )
    parser :: Parser Options
    parser = case oType of
      WithGarnerTs garnerConfig -> WithGarnerTsOpts garnerConfig <$> withGarnerTsParser (targets garnerConfig)
      WithoutGarnerTs -> WithoutGarnerTsOpts <$> withoutGarnerTsParser
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Develop, build, and test your projects reliably and easily"
            <> header "garner - the project manager"
            <> footerDoc (Just unavailable)
        )

data OptionType
  = WithGarnerTs GarnerConfig
  | WithoutGarnerTs
  deriving stock (Eq, Show)

data Options
  = WithGarnerTsOpts GarnerConfig WithGarnerTsCommand
  | WithoutGarnerTsOpts WithoutGarnerTsCommand

data WithoutGarnerTsCommand
  = Init

data WithGarnerTsCommand
  = Gen
  | Run CommandOptions
  | Enter CommandOptions
  | Ci CommandOptions
  deriving stock (Eq, Show)

withGarnerTsCommandInfo :: [(String, String, Targets -> Parser WithGarnerTsCommand)]
withGarnerTsCommandInfo =
  [ ("run", "Build and run the default executable of a target", withCommandOptions Run),
    ("enter", "Enter a devshell for a target", withCommandOptions Enter),
    ("gen", "Generate the flake.nix file and exit", const $ pure Gen),
    ("ci", "Run the garnix ci tests locally", withCommandOptions Ci)
  ]
  where
    withCommandOptions constructor target =
      constructor <$> commandOptionsParser target

withGarnerTsParser :: Targets -> Parser WithGarnerTsCommand
withGarnerTsParser targets =
  subparser $
    mconcat
      [ OA.command cmd (info (runner targets) (progDesc desc))
        | (cmd, desc, runner) <- withGarnerTsCommandInfo
      ]

withouGarnerTsCommandInfo :: [(String, String, Parser WithoutGarnerTsCommand)]
withouGarnerTsCommandInfo =
  [("init", "Infer a garner.ts file from the project layout", pure Init)]

withoutGarnerTsParser :: Parser WithoutGarnerTsCommand
withoutGarnerTsParser =
  subparser $
    mconcat
      [ OA.command cmd (info runner (progDesc desc))
        | (cmd, desc, runner) <- withouGarnerTsCommandInfo
      ]

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

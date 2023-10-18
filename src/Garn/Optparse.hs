{-# LANGUAGE DuplicateRecordFields #-}

module Garn.Optparse
  ( getOpts,
    Options (..),
    OptionType (..),
    WithGarnTsCommand (..),
    WithoutGarnTsCommand (..),
    CommandOptions (..),
    CheckCommandOptions (..),
  )
where

import qualified Data.Map as Map
import Garn.GarnConfig
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
                    WithGarnTs _ -> formatCommands withouGarnTsCommandInfo
                    WithoutGarnTs -> formatCommands withGarnTsCommandInfo
              )
    parser :: Parser Options
    parser = case oType of
      WithGarnTs garnConfig -> WithGarnTsOpts garnConfig <$> withGarnTsParser (targets garnConfig)
      WithoutGarnTs -> WithoutGarnTsOpts <$> withoutGarnTsParser
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Develop, build, and test your projects reliably and easily"
            <> header "garn - the project manager"
            <> footerDoc (Just unavailable)
        )

data OptionType
  = WithGarnTs GarnConfig
  | WithoutGarnTs
  deriving stock (Eq, Show)

data Options
  = WithGarnTsOpts GarnConfig WithGarnTsCommand
  | WithoutGarnTsOpts WithoutGarnTsCommand

data WithoutGarnTsCommand
  = Init

data WithGarnTsCommand
  = Gen
  | Build CommandOptions
  | Run CommandOptions [String]
  | Enter CommandOptions
  | Check CheckCommandOptions
  deriving stock (Eq, Show)

withGarnTsCommandInfo :: [(String, String, Targets -> Parser WithGarnTsCommand)]
withGarnTsCommandInfo =
  [ ("build", "Build the default executable of a project", withCommandOptions Build),
    ("run", "Build and run the default executable of a project", withCommandOptionsAndArgv Run),
    ("enter", "Enter the default devshell for a project", withCommandOptions Enter),
    ("generate", "Generate the flake.nix file and exit", const $ pure Gen),
    ("check", "Run the checks of a project", checkCommand)
  ]
  where
    withCommandOptions constructor targets =
      constructor <$> commandOptionsParser targets
    withCommandOptionsAndArgv constructor targets =
      constructor <$> commandOptionsParser targets <*> argvParser
    checkCommand :: Targets -> Parser WithGarnTsCommand
    checkCommand targets =
      let checkCommandOptions =
            Qualified <$> commandOptionsParser targets
              <|> pure Unqualified
       in Check <$> checkCommandOptions

argvParser :: Parser [String]
argvParser = many $ strArgument $ metavar "...args"

withGarnTsParser :: Targets -> Parser WithGarnTsCommand
withGarnTsParser targets =
  subparser $
    mconcat
      [ OA.command cmd (info (runner targets) (progDesc desc))
        | (cmd, desc, runner) <- withGarnTsCommandInfo
      ]

withouGarnTsCommandInfo :: [(String, String, Parser WithoutGarnTsCommand)]
withouGarnTsCommandInfo =
  [("init", "Infer a garn.ts file from the project layout", pure Init)]

withoutGarnTsParser :: Parser WithoutGarnTsCommand
withoutGarnTsParser =
  subparser $
    mconcat
      [ OA.command cmd (info runner (progDesc desc))
        | (cmd, desc, runner) <- withouGarnTsCommandInfo
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
                  (progDesc $ getDescription targetConfig)
              )
        )
        $ Map.assocs targets
    )
    <**> helper

data CheckCommandOptions
  = Qualified CommandOptions
  | Unqualified
  deriving stock (Eq, Show)

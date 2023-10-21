{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}

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

import Control.Arrow (second)
import qualified Data.Map as Map
import Garn.GarnConfig
import Options.Applicative hiding (command)
import qualified Options.Applicative as OA
import Options.Applicative.Common (runParser)
import qualified Options.Applicative.Help.Pretty as OA
import Options.Applicative.Internal (runP)
import Options.Applicative.Types (IsCmdStart (CmdStart))
import System.Environment (getArgs)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

getOpts :: OptionType -> IO Options
getOpts oType = do
  allArgs <- getArgs
  let (optParseArgs, drop 1 -> leftOverAfterDoubleDash) = span (/= "--") allArgs
  -- We run `runParser` which will apply the parser, but give us any leftover
  -- arguments it cannot parse.
  let (resultWithoutLeftOversAfterDoubleDash, contexts) =
        runP
          (runParser (infoPolicy parserInfo) CmdStart (infoParser parserInfo) optParseArgs)
          parserPrefs
      result =
        fmap (second (<> leftOverAfterDoubleDash)) resultWithoutLeftOversAfterDoubleDash
  case result of
    Right (WithGarnTsOpts config (Run commandOptions []), leftOverAfterRunCommand) ->
      pure $ WithGarnTsOpts config (Run commandOptions leftOverAfterRunCommand)
    Right (WithGarnTsOpts _config (Run _commandOptions (_ : _)), _rest) ->
      error "BUG: parser shouldn't include any leftover args for run commands!"
    Right (options, []) -> pure options
    Right (_options, _ : _) -> do
      -- This is a parse error, since only `run` commands can have arbitrary
      -- leftover arguments. To get the normal error behavior, we parse the
      -- flags again, this time with `execParserPure`, which processes all
      -- arguments.
      handleParseResult $ execParserPure parserPrefs parserInfo allArgs
    Left err ->
      handleParseResult $ Failure $ parserFailure parserPrefs parserInfo err contexts
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

    parserPrefs :: ParserPrefs
    parserPrefs = prefs $ showHelpOnError <> showHelpOnEmpty

    parser :: Parser Options
    parser = case oType of
      WithGarnTs garnConfig -> WithGarnTsOpts garnConfig <$> withGarnTsParser (targets garnConfig)
      WithoutGarnTs -> WithoutGarnTsOpts <$> withoutGarnTsParser

    parserInfo :: ParserInfo Options
    parserInfo =
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
  deriving stock (Show)

data WithoutGarnTsCommand
  = Init
  deriving stock (Show)

data WithGarnTsCommand
  = Gen
  | Build CommandOptions
  | Run CommandOptions [String]
  | Enter CommandOptions
  | Check CheckCommandOptions
  deriving stock (Eq, Show)

withGarnTsCommandInfo :: [(String, String, Targets -> Parser WithGarnTsCommand)]
withGarnTsCommandInfo =
  [ ("build", "Build the default executable of a project", buildCommand),
    ("run", "Build and run the default executable of a project", runCommand),
    ("enter", "Enter the default devshell for a project", enterCommand),
    ("generate", "Generate the flake.nix file and exit", const $ pure Gen),
    ("check", "Run the checks of a project", checkCommand)
  ]

buildCommand :: Targets -> Parser WithGarnTsCommand
buildCommand targets =
  Build <$> commandOptionsParser (Map.filter isProject targets)

runCommand :: Targets -> Parser WithGarnTsCommand
runCommand targets =
  Run <$> commandOptionsParser targets <*> pure []

enterCommand :: Targets -> Parser WithGarnTsCommand
enterCommand targets =
  Enter <$> commandOptionsParser (Map.filter isProject targets)

checkCommand :: Targets -> Parser WithGarnTsCommand
checkCommand targets =
  let checkCommandOptions =
        Qualified <$> commandOptionsParser (Map.filter isProject targets)
          <|> pure Unqualified
   in Check <$> checkCommandOptions

isProject :: TargetConfig -> Bool
isProject = \case
  TargetConfigProject _ -> True
  TargetConfigExecutable _ -> False

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

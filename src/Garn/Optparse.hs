{-# LANGUAGE DuplicateRecordFields #-}

module Garn.Optparse
  ( getOpts,
    Options (..),
    OptionType (..),
    WithGarnTsCommand (..),
    WithoutGarnTsCommand (..),
    AlwaysCommand (..),
    CommandOptions (..),
    CheckCommandOptions (..),
  )
where

import Control.Exception (throw)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Garn.Common (garnCliVersion)
import qualified Garn.Errors
import Garn.GarnConfig
import Options.Applicative hiding (command)
import qualified Options.Applicative as OA
import qualified Options.Applicative.Help.Pretty as OA
import qualified Text.PrettyPrint.ANSI.Leijen as PP

getOpts :: OptionType -> IO Options
getOpts oType =
  customExecParser (prefs $ subparserInline <> showHelpOnError <> showHelpOnEmpty) opts
  where
    unavailable :: OA.Doc
    unavailable =
      let fst3 (a, _, _) = a
          formatCommands = fmap PP.string
       in PP.nest
            2
            $ PP.vsep
              ( PP.string "Unavailable commands:"
                  : case oType of
                    WithGarnTs _ -> formatCommands $ fst3 <$> withoutGarnTsCommandInfo
                    WithoutGarnTs -> formatCommands $ fst3 <$> withGarnTsCommandInfo
                    BrokenGarnTs _ ->
                      formatCommands
                        ( (fst3 <$> withGarnTsCommandInfo)
                            ++ (fst3 <$> withoutGarnTsCommandInfo)
                        )
              )
    parser :: Parser Options
    parser = case oType of
      WithGarnTs garnConfig ->
        (WithGarnTsOpts garnConfig <$> withGarnTsParser (targets garnConfig))
          <|> (AlwaysAvailableOpts <$> alwaysParser)
      WithoutGarnTs ->
        (WithoutGarnTsOpts <$> withoutGarnTsParser)
          <|> (AlwaysAvailableOpts <$> alwaysParser)
      BrokenGarnTs err ->
        (AlwaysAvailableOpts <$> alwaysParser)
          <|> throw (Garn.Errors.UserError $ errorMessage err)
    version =
      infoOption garnCliVersion $
        mconcat [long "version", help ("Show garn version (" <> garnCliVersion <> ")")]
    opts =
      info
        (parser <**> helper <**> version)
        ( fullDesc
            <> progDesc "Develop, build, and test your projects reliably and easily"
            <> header "garn - the project manager"
            <> footerDoc (Just unavailable)
        )

data OptionType
  = WithGarnTs GarnConfig
  | WithoutGarnTs
  | BrokenGarnTs ReadConfigError
  deriving stock (Eq, Show)

data Options
  = WithGarnTsOpts GarnConfig WithGarnTsCommand
  | WithoutGarnTsOpts WithoutGarnTsCommand
  | AlwaysAvailableOpts AlwaysCommand

data WithoutGarnTsCommand
  = Init

data WithGarnTsCommand
  = Gen
  | Build CommandOptions
  | Run CommandOptions [String]
  | Enter CommandOptions
  | Check CheckCommandOptions
  deriving stock (Eq, Show)

data AlwaysCommand
  = Edit [String]
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
  Build <$> commandOptionsParser (Map.filter isBuildable targets)
  where
    isBuildable = \case
      TargetConfigProject _ -> True
      TargetConfigEnvironment _ -> False
      TargetConfigPackage _ -> True
      TargetConfigExecutable _ -> False

runCommand :: Targets -> Parser WithGarnTsCommand
runCommand targets =
  Run <$> commandOptionsParser (Map.filter isRunnable targets) <*> argvParser
  where
    argvParser :: Parser [String]
    argvParser = many $ strArgument $ metavar "...args"

    isRunnable :: TargetConfig -> Bool
    isRunnable = \case
      TargetConfigProject projectTarget -> runnable projectTarget
      TargetConfigEnvironment _ -> False
      TargetConfigPackage _ -> False
      TargetConfigExecutable _ -> True

enterCommand :: Targets -> Parser WithGarnTsCommand
enterCommand targets =
  Enter <$> commandOptionsParser (Map.filter isEnterable targets)
  where
    isEnterable = \case
      TargetConfigProject _ -> True
      TargetConfigEnvironment _ -> True
      TargetConfigPackage _ -> False
      TargetConfigExecutable _ -> False

checkCommand :: Targets -> Parser WithGarnTsCommand
checkCommand targets =
  let checkCommandOptions =
        Qualified
          <$> commandOptionsParser (Map.filter isProject targets)
            <|> pure Unqualified
   in Check <$> checkCommandOptions

isProject :: TargetConfig -> Bool
isProject = \case
  TargetConfigProject _ -> True
  TargetConfigEnvironment _ -> False
  TargetConfigPackage _ -> False
  TargetConfigExecutable _ -> False

withGarnTsParser :: Targets -> Parser WithGarnTsCommand
withGarnTsParser targets =
  subparser $
    mconcat
      [ OA.command cmd (info (runner targets) (progDesc desc))
        | (cmd, desc, runner) <- withGarnTsCommandInfo
      ]

withoutGarnTsCommandInfo :: [(String, String, Parser WithoutGarnTsCommand)]
withoutGarnTsCommandInfo =
  [("init", "Infer a garn.ts file from the project layout", pure Init)]

withoutGarnTsParser :: Parser WithoutGarnTsCommand
withoutGarnTsParser =
  subparser $
    mconcat
      [ OA.command cmd (info runner (progDesc desc))
        | (cmd, desc, runner) <- withoutGarnTsCommandInfo
      ]

alwaysParser :: Parser AlwaysCommand
alwaysParser =
  subparser $
    OA.command "edit" (info (Edit <$> argvParser) (progDesc desc)) <> hidden
  where
    argvParser :: Parser [String]
    argvParser = many $ strArgument $ metavar "...args"

    desc = "Edit garn.ts in VSCodium with Deno integration set up"

data CommandOptions = CommandOptions
  { target :: TargetName,
    targetConfig :: TargetConfig
  }
  deriving stock (Eq, Show)

commandOptionsParser :: Targets -> Parser CommandOptions
commandOptionsParser targets =
  subparser
    ( foldMap
        ( \(target, targetConfig) ->
            OA.command
              (asUserFacing target)
              ( info
                  (pure (CommandOptions target targetConfig))
                  (maybe mempty progDesc (getDescription targetConfig))
              )
        )
        $ Map.assocs targets
    )
    <**> helper

data CheckCommandOptions
  = Qualified CommandOptions
  | Unqualified
  deriving stock (Eq, Show)

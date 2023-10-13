{-# LANGUAGE QuasiQuotes #-}

module Garn.CodeGen
  ( run,
    fromToplevelDerivation,
  )
where

import Data.Aeson (FromJSON, eitherDecode, toJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Map (Map, toAscList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Development.Shake
import GHC.Generics (Generic)
import Garn.Common (currentSystem, nixpkgsInput)
import WithCli (withCli)

run :: IO ()
run = withCli $ do
  system <- currentSystem
  let varName = "pkgs"
      nixpkgsExpression =
        [i|
          import (builtins.getFlake "#{nixpkgsInput}") {
            system = "#{system}";
            config.allowAliases = false;
          }
        |]
  code <- fromToplevelDerivation "." varName nixpkgsExpression
  writeFile "ts/nixpkgs.ts" code

fromToplevelDerivation :: String -> String -> String -> IO String
fromToplevelDerivation garnLibRoot varName rootExpr = do
  system :: String <- do
    Stdout json <- cmd "nix" nixArgs "eval --impure --json --expr builtins.currentSystem"
    pure $ either error id $ eitherDecode json
  Stdout json <- cmd "nix" nixArgs "eval" (".#lib." <> system) "--json --apply" [nixExpr]
  pkgs :: Map String PkgInfo <- case eitherDecode json of
    Right pkgs -> pure pkgs
    Left e -> error (e <> " in " <> cs json)
  let sanitizedPkgs = Map.mapKeys sanitize pkgs
  pure $
    unindent
      [i|
        import { mkPackage } from "#{garnLibRoot}/package.ts";
        import { nixRaw } from "#{garnLibRoot}/nix.ts";

      |]
      <> pkgsString varName sanitizedPkgs
  where
    nixExpr =
      [i|
        lib :
        let mk = name: value: {
              attribute = name;
              description = if value ? meta.description
                then value.meta.description
                else null;
            };
            isNotBroken = value:
                let broken = (builtins.tryEval (value.meta.broken or false));
                in broken.success && !broken.value;
            doesNotThrow = value : (builtins.tryEval value).success;
            filterAttrs = lib.attrsets.filterAttrs
              (name: value:
                    doesNotThrow value
                && lib.isDerivation value
                && isNotBroken value);
        in
          (lib.mapAttrs mk
            (filterAttrs (#{rootExpr}))
          )
      |]

data PkgInfo = PkgInfo
  { description :: Maybe String,
    attribute :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

pkgDoc :: PkgInfo -> String
pkgDoc pkgInfo = case description pkgInfo of
  Nothing -> ""
  Just doc ->
    unindent
      [i|
        /**
         * #{doc}
         */
      |]

formatPkg :: String -> (String, PkgInfo) -> String
formatPkg varName (name, pkgInfo) =
  let escapedDoc = encodeToLazyText . toJSON $ fromMaybe "" $ description pkgInfo
   in pkgDoc pkgInfo
        <> unindent
          [i|
            export const #{name} = mkPackage(
              nixRaw`#{varName}.#{attribute pkgInfo}`,
              #{escapedDoc},
            );
          |]

pkgsString :: String -> Map String PkgInfo -> String
pkgsString varName pkgs =
  intercalate "\n" $ formatPkg varName <$> toAscList pkgs

sanitize :: String -> String
sanitize str
  | str `elem` tsKeywords = str <> "_"
  | otherwise =
      str <&> \case
        '-' -> '_'
        x -> x

tsKeywords :: [String]
tsKeywords =
  [ "arguments", -- Only in strict mode
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceOf",
    "new",
    "null",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeOf",
    "var",
    "void",
    "while",
    "with"
  ]

nixArgs :: [String]
nixArgs = ["--extra-experimental-features", "flakes nix-command"]

{-# LANGUAGE QuasiQuotes #-}

module Garner.CodeGen
  ( run,
    fromToplevelDerivation,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Map (Map, toAscList)
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Development.Shake
import GHC.Generics (Generic)
import WithCli (withCli)

run :: IO ()
run = withCli $ do
  StdoutTrim (system :: String) <- cmd "nix eval --impure --raw --expr builtins.currentSystem"
  let varName = "pkgs"
      nixpkgsExpression =
        [i|
          let commit = "23.05";
              sha256 = "10wn0l08j9lgqcw8177nh2ljrnxdrpri7bp0g7nvrsn9rkawvlbf";
              pkgsSrc = builtins.fetchTarball {
                url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
                inherit sha256;
              };
          in import pkgsSrc { system = "#{system}"; }
        |]
  code <- fromToplevelDerivation "." varName nixpkgsExpression
  writeFile "ts/nixpkgs.ts" code

fromToplevelDerivation :: String -> String -> String -> IO String
fromToplevelDerivation garnerLibRoot varName rootExpr = do
  system :: String <- do
    Stdout json <- cmd "nix eval --impure --json --expr builtins.currentSystem"
    pure $ either error id $ eitherDecode json
  Stdout json <- cmd "nix eval" (".#lib." <> system) "--json --apply" [nixExpr]
  pkgs :: Map String PkgInfo <- case eitherDecode json of
    Right pkgs -> pure pkgs
    Left e -> error (e <> " in " <> cs json)
  let sanitizedPkgs = Map.mapKeys sanitize pkgs
  pure $
    unindent
      [i|
        import { mkPackage } from "#{garnerLibRoot}/base.ts";

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
  pkgDoc pkgInfo
    <> unindent
      [i|
        export const #{name} = mkPackage({
          attribute: `#{varName}.#{attribute pkgInfo}`,
        });
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

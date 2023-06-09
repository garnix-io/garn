{-# LANGUAGE QuasiQuotes #-}

module Garner.CodeGen
  ( run,
    fromToplevelDerivation,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import Data.List (intercalate)
import Data.Map (Map, toAscList)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Development.Shake
import GHC.Generics (Generic)
import WithCli (withCli)

run :: IO ()
run = withCli $ \varName rootExpr -> do
  code <- fromToplevelDerivation "." varName rootExpr
  writeFile "ts/nixpkgs.ts" code

fromToplevelDerivation :: String -> String -> String -> IO String
fromToplevelDerivation garnerLibRoot varName rootExpr = do
  system :: String <- do
    Stdout json <- cmd "nix eval --impure --json --expr builtins.currentSystem"
    return $ either error id $ eitherDecode json
  Stdout json <- cmd "nix eval --impure" (".#lib." <> system) "--json --apply" [nixExpr]
  pkgs :: Map String PkgInfo <- case eitherDecode json of
    Right pkgs -> pure pkgs
    Left e -> error (e <> " in " <> cs json)
  return $
    unindent
      [i|
    import { mkDerivation } from "#{garnerLibRoot}/base.ts";

    |]
      <> pkgsString varName pkgs
  where
    nixExpr =
      [i| lib :
          let mk = name: value: {
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
  {description :: Maybe String}
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
            export const #{sanitize name} = mkDerivation({
              attribute: `#{varName}.#{name}`,
            });
        |]

pkgsString :: String -> Map String PkgInfo -> String
pkgsString varName pkgs =
  intercalate "\n" $ formatPkg varName <$> toAscList pkgs

sanitize :: String -> String
sanitize str
  | str `elem` tsKeywords = str <> "_"
  | otherwise =
      let replaceChar '-' = '_'
          replaceChar x = x
       in fmap replaceChar str

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

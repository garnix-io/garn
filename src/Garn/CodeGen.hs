{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Garn.CodeGen
  ( Garn.CodeGen.run,
    PkgInfo (Derivation, Collection, description, path),
    scanPackages,
    writePkgFiles,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (forM_)
import Cradle (StdoutUntrimmed (..), run)
import Data.Aeson (FromJSON, eitherDecode, toJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Map (Map, toAscList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import GHC.Generics (Generic)
import Garn.Common (currentSystem, nixpkgsInput)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import WithCli (withCli)

-- A nix expression that specifies which packages to include in the final
-- collection.
--
-- All top-level derivations included by default, but everything else excluded.
-- This can be overridden with the given attribute set.
pkgSpec :: String
pkgSpec =
  [i|
    {
      haskellPackages = {};
      nimPackages = {};
      nodePackages = { "@antora/cli" = false; };
      phpPackages = {};
      python2Packages = {};
      python3Packages = {};
      rubyPackages = {};
      rustPackages = {};
    }
  |]

pkgs :: String -> String
pkgs system =
  [i|
    import (builtins.getFlake "#{nixpkgsInput}") {
      system = "#{system}";
      config.allowAliases = false;
    }
  |]

run :: IO ()
run = withCli $ do
  system <- currentSystem
  let outDir = "ts/internal/nixpkgs"
  removeDirectoryRecursive outDir `catch` \(_ :: IOException) -> pure ()
  pkgs <- scanPackages system (pkgs system) pkgSpec
  writePkgFiles outDir "../.." pkgs
  writeFile "ts/nixpkgs.ts" "export * from \"./internal/nixpkgs/mod.ts\";\n"

writePkgFiles :: String -> String -> Map String PkgInfo -> IO ()
writePkgFiles modulePath garnLibRoot (Map.mapKeys sanitize -> pkgs) = do
  createDirectoryIfMissing True modulePath
  let code =
        unindent
          [i|
            import { mkPackage } from "#{garnLibRoot}/package.ts";
            import { nixRaw } from "#{garnLibRoot}/nix.ts";

          |]
          <> pkgsString pkgs
  writeFile (modulePath <> "/mod.ts") code
  forM_ (Map.assocs pkgs) $ \(name :: String, pkgInfo :: PkgInfo) -> do
    case pkgInfo of
      Derivation {} -> pure ()
      Collection {subPkgs} -> writePkgFiles (modulePath <> "/" <> name) (garnLibRoot <> "/..") subPkgs

scanPackages :: String -> String -> String -> IO (Map String PkgInfo)
scanPackages system pkgs pkgSpec = do
  StdoutUntrimmed json <- Cradle.run "nix" nixArgs "eval" (".#lib." <> system) "--json" "--apply" nixExpr
  case eitherDecode (cs json) of
    Right pkgs -> pure pkgs
    Left e -> error (e <> " in " <> cs json)
  where
    nixExpr =
      [i|
        lib:
          let
            scan = path: pkgs: pkgSpec:
              if pkgSpec == false then null
              else filterNulls (lib.mapAttrs (k: v:
                  let
                    newPath = "${path}.${k}";
                  in
                    if pkgSpec ? ${k}
                      then mkCollection (scan newPath v (pkgSpec.${k}))
                      else if isWorkingDerivation v then mkDerivation newPath v
                      else null
                ) pkgs);
            mkCollection = subPkgs:
              if subPkgs == null then null
              else {
                tag = "Collection";
                inherit subPkgs;
              };
            mkDerivation = path: pkg: {
                tag = "Derivation";
                inherit path;
                description = if pkg ? meta.description
                  then pkg.meta.description
                  else null;
              };
            filterNulls = lib.filterAttrs (k: v: v != null);
            isWorkingDerivation = value:
              (builtins.tryEval value).success &&
              (let
                evalResult = (builtins.tryEval (value.meta.broken or false));
               in
                evalResult.success &&
                !evalResult.value &&
                lib.isDerivation value);
          in
            scan "pkgs" (#{pkgs}) (#{pkgSpec})
      |]

data PkgInfo
  = Derivation {description :: Maybe String, path :: String}
  | Collection {subPkgs :: Map String PkgInfo}
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

formatPkg :: (String, PkgInfo) -> String
formatPkg (name, pkgInfo) = do
  case pkgInfo of
    Derivation {description, path} ->
      let escapedDoc = encodeToLazyText . toJSON $ fromMaybe "" description
       in pkgDoc pkgInfo
            <> unindent
              [i|
                export const #{name} = mkPackage(
                  nixRaw`#{path}`,
                  #{escapedDoc},
                );
              |]
    Collection _ ->
      unindent
        [i|
          export * as #{name} from "./#{name}/mod.ts";
        |]

pkgsString :: Map String PkgInfo -> String
pkgsString pkgs =
  intercalate "\n" $ formatPkg <$> toAscList pkgs

sanitize :: String -> String
sanitize str
  | isDigit $ head str = sanitize $ "_" <> str
  | str `elem` tsKeywords = str <> "_"
  | otherwise =
      str <&> \case
        '+' -> '_'
        '-' -> '_'
        '.' -> '_'
        '/' -> '_'
        '@' -> '_'
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
    "interface",
    "new",
    "null",
    "private",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeOf",
    "typeof",
    "var",
    "void",
    "while",
    "with"
  ]

nixArgs :: [String]
nixArgs = ["--extra-experimental-features", "flakes nix-command"]

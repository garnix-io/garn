{-# LANGUAGE QuasiQuotes #-}

module Garn.CodeGen
  ( Garn.CodeGen.run,
    PkgInfo (Derivation, Collection, description, path, subPkgs),
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

-- A specification for what packages to include in the final collection
-- collectionSpec represents a colleciton with all derivations included by
-- default, but everything else excluded. This can be overridden with the
-- attribute set.
pkgSpec :: String
pkgSpec =
  [i|
    collectionSpec {
      haskellPackages = collectionSpec {};
      nimPackages = collectionSpec {};
      nodePackages = collectionSpec { "@antora/cli" = false; };
      phpPackages = collectionSpec {};
      python2Packages = collectionSpec {};
      python3Packages = collectionSpec {};
      rubyPackages = collectionSpec {};
      rustPackages = collectionSpec {};
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

writePkgFiles :: String -> String -> Map String PkgInfo -> IO ()
writePkgFiles modulePath garnLibRoot pkgs = do
  createDirectoryIfMissing True modulePath
  let sanitizedPkgs = Map.mapKeys sanitize pkgs
  let code =
        unindent
          [i|
            import { mkPackage } from "#{garnLibRoot}/package.ts";
            import { nixRaw } from "#{garnLibRoot}/nix.ts";

          |]
          <> pkgsString sanitizedPkgs
  writeFile (modulePath <> "/mod.ts") code
  forM_ (Map.assocs sanitizedPkgs) $ \(name :: String, pkgInfo :: PkgInfo) -> do
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
            collectionSpec = overrides: { inherit overrides; };
            filterNulls = lib.filterAttrs (k: v: v != null);
            scan = path: pkgs: pkgSpec:
              filterNulls (lib.mapAttrs (k: v:
                let
                  newPath = "${path}.${k}";
                  spec = if pkgSpec.overrides ? ${k}
                    then pkgSpec.overrides.${k}
                    else if isBroken v then false
                    else lib.isDerivation v;
                in
                  if spec == false then null
                  else if spec == true then mkDerivation newPath v
                  else mkCollection (scan newPath v spec)
              ) pkgs);

            mkCollection = subPkgs: {
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
            isBroken = value: !(builtins.tryEval value).success
              || !(let evalResult = (builtins.tryEval (value.meta.broken or false));
                 in evalResult.success && !evalResult.value);
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

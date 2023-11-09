{-# LANGUAGE QuasiQuotes #-}

module Garn.CodeGen
  ( Garn.CodeGen.run,
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

run :: IO ()
run = withCli $ do
  system <- currentSystem
  let outDir = "ts/internal/nixpkgs"
  removeDirectoryRecursive outDir `catch` \(_ :: IOException) -> pure ()
  pkgs <- scanPackages system
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

scanPackages :: String -> IO (Map String PkgInfo)
scanPackages system = do
  StdoutUntrimmed json <- Cradle.run "nix" nixArgs "eval" (".#lib." <> system) "--json" "--apply" nixExpr
  case eitherDecode (cs json) of
    Right pkgs -> pure pkgs
    Left e -> error (e <> " in " <> cs json)
  where
    nixExpr =
      [i|
        let pkgs = import (builtins.getFlake "#{nixpkgsInput}") {
          system = "#{system}";
          config.allowAliases = false;
        };
        in lib:
          let debug = false;
              trace = path: value: if debug then builtins.trace path value else value;
              brokenPkgs = {
                __splicedPackages = true;
                beam = { interpreters = true; packages = true; };
                beam_minimal = { interpreters = true; packages = true; };
                beam_nox = { interpreters = true; packages = true; };
                buildPackages = true;
                cudaPackages_10 = true;
                cudaPackages_10_0 = true;
                cudaPackages_10_1 = true;
                cudaPackages_10_2 = true;
                cudaPackages_11_0 = true;
                cudaPackages_11_1 = true;
                cudaPackages_11_2 = true;
                cudaPackages_11_3 = true;
                dockapps = true;
                lib = true;
                nixosTests = true;
                nodePackages = { "@antora/cli" = true; };
                nodePackages_latest = { "@antora/cli" = true; };
                pkgs = true;
                pkgsBuildBuild = true;
                pkgsBuildHost = true;
                pkgsBuildTarget = true;
                pkgsCross = true;
                pkgsHostHost = true;
                pkgsHostTarget = true;
                pkgsLLVM = true;
                pkgsMusl = true;
                pkgsStatic = true;
                pkgsTargetTarget = true;
                pkgsi686Linux = true;
                pypy27Packages = true;
                pypy2Packages = true;
                pypy310Packages = true;
                pypy39Packages = true;
                pypy3Packages = true;
                pypyPackages = true;
                swiftPackages = { darwin = true; };
                targetPackages = true;
                tests = true;
              };
              isBroken = broken: name: value:
                (broken ? ${name} && broken.${name} == true)
                || !(builtins.tryEval value).success
                || !(let evalResult = (builtins.tryEval (value.meta.broken or false));
                   in evalResult.success && !evalResult.value);
              getBroken = broken: subPkgName: if broken ? ${subPkgName} then broken.${subPkgName} else {};
              mkDerivation = path: pkg: {
                  tag = "Derivation";
                  path = path;
                  description = if pkg ? meta.description
                    then pkg.meta.description
                    else null;
                };
              mkCollection = maxDepth: broken: path: collection:
                let
                  subPkgs = scan (maxDepth - 1) broken path collection;
                in if builtins.length (builtins.attrNames subPkgs) == 0
                  then null
                  else {
                    tag = "Collection";
                    inherit subPkgs;
                  };
              mkEntry = maxDepth: broken: path: name: value:
                let newPath = "${path}.${name}"; in
                if lib.isDerivation value then trace "derivation ${newPath}" mkDerivation newPath value
                else if lib.isAttrs value then trace "collection ${newPath}" (mkCollection maxDepth (getBroken broken name) newPath value)
                else null;
              scan = maxDepth: broken: path: root: 
                let
                  working = lib.filterAttrs (k: v: !(isBroken broken k v)) root;
                in
                  if maxDepth > 0
                    then lib.filterAttrs (k: v: v != null) (lib.mapAttrs (mkEntry maxDepth broken path) working)
                    else {};
          in
            scan 2 brokenPkgs "pkgs" pkgs
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

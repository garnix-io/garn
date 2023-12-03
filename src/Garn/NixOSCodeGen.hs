{-# LANGUAGE QuasiQuotes #-}

module Garn.NixOSCodeGen
  ( getServices,
    generateTS,
    Service (..),
    Option (..),
    SimpleOption (..),
  )
where

import Control.Monad
import Cradle
import Data.Aeson
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util
import GHC.Generics (Generic)
import Garn.Common (currentSystem, nixpkgsInput)
import System.Exit (ExitCode (..))

generateTS :: [Service] -> String
generateTS services =
  unindent
    [i|
    class Services {
        private config: Record<string, Object>;

        constructor() {
            this.config = {}
        }
        #{setters}
    }|]
  where
    mkType (Option option) = Map.foldrWithKey mk "" option
      where
        mk :: String -> SimpleOption -> String -> String
        mk name opt acc = acc <> ", " <> name <> ": " <> simpleOptionType opt
    go service =
      [i|
       set #{name service}(args: {mkType $ options service}) {
           this.config.#{name service} = args;
       }
    |]
    setters = concatMap go services

getServices :: IO [Service]
getServices = do
  (exit, Stderr err, StdoutUntrimmed json) <- Cradle.run "nix" nixArgs "eval" ".#" "--json" "--apply" nixExpr
  unless (exit == ExitSuccess) $ do
    error ("err:" <> cs err)
  writeFile "out.json" (cs json)
  case eitherDecode (cs json) of
    Right pkgs -> pure pkgs
    Left e -> error (e <> " in ")
  where
    nixExpr :: String
    nixExpr =
      [i| id:
       let pkgs = (#{pkgs});
           nixos = pkgs.nixos ({...}: { });
           getOpts = builtins.mapAttrs (optName: optDef:              {
             "description" = if optDef ? description
               then optDef.description
               else "";
             "optionType" = optDef.type.name;
           });
           removeBads = pkgs.lib.filterAttrs (optName: optDef:
             optDef ? "type" && optDef.type ? name);

           go = pkgs.lib.mapAttrsToList (serviceName: def:              {

             "name" = serviceName;
             "options" =
               let e = getOpts (removeBads def);
               in builtins.tryEval e;
           });
           filterBads = pkgs.lib.filter (e: e.options.success);
           un = builtins.map (e: {
              name = e.name;
              options = e.options.value;
           });

       in (un (filterBads (go nixos.options.services)))
    |]

pkgs :: String
pkgs =
  [i|
    let pkgs = import (builtins.getFlake "#{nixpkgsInput}") {
      system = "x86_64-linux";
      config.allowAliases = false;
    };
    in pkgs
  |]

data Service = Service
  { name :: String,
    options :: Option
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype Option = Option
  { getOption :: Map.Map String SimpleOption
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Monoid, Semigroup)

data SimpleOption = SimpleOption
  { optionType :: String,
    description :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

nixArgs :: [String]
nixArgs = ["--extra-experimental-features", "flakes nix-command"]

simpleOptionType :: SimpleOption -> String
simpleOptionType s = case lookup (optionType s) typeMappings of
  Nothing -> "any"
  Just r -> r

-- (NixType, TypescriptType) pairs.
-- Taken from `builtins.attrNames pkgs.lib.types`
typeMappings :: [(String, String)]
typeMappings =
  [ -- "addCheck"
    ("anything", "any"),
    -- "attrs"
    -- "attrsOf"
    ("bool", "boolean"),
    -- "coercedTo"
    -- "commas"
    -- "defaultFunctor"
    -- "defaultTypeMerge"
    -- "deferredModule"
    -- "deferredModuleWith"
    -- "either"
    -- "enum"
    -- "envVar"
    ("float", "number"),
    -- "functionTo"
    ("int", "number"),
    -- "ints"
    -- "isOptionType"
    -- "isType"
    -- "lazyAttrsOf"
    -- "lines"
    -- "listOf"
    -- "loaOf"
    -- "mkOptionType"
    -- "nonEmptyListOf"
    -- "nonEmptyStr"
    -- "nullOr"
    ("number", "number"),
    -- "numbers"
    -- "oneOf"
    -- "optionDescriptionPhrase"
    -- "optionType"
    -- "package"
    -- "passwdEntry"
    -- "path"
    -- "pathInStore"
    -- "pkgs"
    -- "port"
    -- "raw"
    -- "separatedString"
    -- "setType"
    -- "shellPackage"
    -- "singleLineStr"
    ("str", "string"),
    -- "strMatching"
    ("string", "string")
    -- "submodule"
    -- "submoduleWith"
    -- "types"
    -- "uniq"
    -- "unique"
    -- "unspecified"
  ]

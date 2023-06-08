{-# LANGUAGE QuasiQuotes #-}

module Garner.CodeGen
  ( fromToplevelDerivation,
  )
where

import Data.Aeson (eitherDecode)
import Data.String.Interpolate (i)
import Development.Shake

fromToplevelDerivation :: String -> String -> IO String
fromToplevelDerivation varName rootExpr = do
  system :: String <- do
    Stdout json <- cmd "nix eval --impure --json --expr builtins.currentSystem"
    return $ either error id $ eitherDecode json
  Stdout json <- cmd "nix eval" (".#lib." <> system) "--json --apply" [nixExpr]
  case eitherDecode json of
    Right tsCode -> pure tsCode
    Left e -> error e
  where
    nixExpr =
      [i| lib :
          let root = #{rootExpr};
              mkDoc = value:
                if value ? meta.description
                then ''
                  /**
                   * ${value.meta.description}
                   */
                ''
                else "";
              mk = name: value: mkDoc value + ''
                export const ${name} = mkDerivation({
                  attribute = `''${root}.${name}`;
                })
              '';
              pickDerivations = lib.attrsets.filterAttrs
                (name : value : (value.type or null) == "derivation");
              removeThrowing = lib.attrsets.filterAttrs
                (name : maybeThrowing : (builtins.tryEval maybeThrowing).success);
          in
          "const root = \\\"#{varName}\\\";\n\n" +
          lib.strings.concatStrings
            (lib.mapAttrsToList mk
              (pickDerivations
                (removeThrowing root)
              )
            )
    |]

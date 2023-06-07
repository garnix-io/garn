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
  Stdout json <- cmd "nix eval --json --expr" [nixExpr]
  case eitherDecode json of
    Right tsCode -> pure tsCode
    Left e -> error e
  where
    nixExpr =
      [i| let root = #{rootExpr};
              mkDoc = value:
                if value ? meta.description
                then ''
                  /**
                   * ${value.meta.description}
                   */
                ''
                else "";

              mkAll = name: value: mkDoc value + ''
                export const ${name} = mkDerivation({
                  attribute = `''${root}.${name}`;
                })
              '';
              mk = name: value:
                if (value.type or "") == "derivation"
                then mkAll name value
                else "";
          in
          "const root = \\\"#{varName}\\\";\n\n" +
          builtins.concatStringsSep "" (builtins.attrValues (builtins.mapAttrs mk root))
    |]

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
              go = attr: ''
                export const ${attr} = mkDerivation({
                  attribute = `''${root}.${attr}`;
                })
              '';
          in
          "const root = \\\"#{varName}\\\";\n\n" +
          builtins.concatStringsSep "\n" (map go (builtins.attrNames root))
    |]

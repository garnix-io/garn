{-# LANGUAGE QuasiQuotes #-}

module Garner.CodeGen
  ( run,
    fromToplevelDerivation,
  )
where

import Data.Aeson (eitherDecode)
import Data.String.Interpolate (i)
import Development.Shake
import WithCli (withCli)

run :: IO ()
run = withCli $ \varName rootExpr -> do
  code <- fromToplevelDerivation varName rootExpr
  writeFile "ts/nixpkgs.ts" code

fromToplevelDerivation :: String -> String -> IO String
fromToplevelDerivation varName rootExpr = do
  system :: String <- do
    Stdout json <- cmd "nix eval --impure --json --expr builtins.currentSystem"
    return $ either error id $ eitherDecode json
  Stdout json <- cmd "nix eval --impure" (".#lib." <> system) "--json --apply" [nixExpr]
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
          "const root = \\\"#{varName}\\\";\n\n" +
          lib.strings.concatStrings
            (lib.mapAttrsToList mk
              (filterAttrs root)
            )
    |]

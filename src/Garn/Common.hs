module Garn.Common (nixpkgsInput, nixArgs, currentSystem) where

import Data.Aeson (eitherDecode)
import Development.Shake (Stdout (..), cmd)

-- | pinned to master on 2023-10-26
nixpkgsInput :: String
nixpkgsInput = "github:NixOS/nixpkgs/6fc7203e423bbf1c8f84cccf1c4818d097612566"

nixArgs :: [String]
nixArgs =
  [ "--extra-experimental-features",
    "flakes nix-command",
    "--print-build-logs"
  ]

currentSystem :: IO String
currentSystem = do
  Stdout json <- cmd "nix" nixArgs "eval --impure --json --expr builtins.currentSystem"
  pure $ either error id $ eitherDecode json

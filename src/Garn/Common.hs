module Garn.Common (nixpkgsInput, nixArgs, currentSystem) where

import Data.Aeson (eitherDecode)
import Development.Shake (Stdout (..), cmd)

-- | pinned to release-23.05 on 2023-10-21
nixpkgsInput :: String
nixpkgsInput = "github:NixOS/nixpkgs/21443a102b1a2f037d02e1d22e3e0ffdda2dbff9"

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

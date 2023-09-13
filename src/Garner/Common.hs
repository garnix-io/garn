module Garner.Common (nixpkgsInput, nixArgs) where

-- | pinned to release-23.05 on 2023-08-31
nixpkgsInput :: String
nixpkgsInput = "github:NixOS/nixpkgs/841889913dfd06a70ffb39f603e29e46f45f0c1a"

nixArgs :: [String]
nixArgs =
  [ "--extra-experimental-features",
    "flakes nix-command",
    "--print-build-logs"
  ]

{-# LANGUAGE TemplateHaskell #-}

module Garn.Common
  ( garnCliVersion,
    Garn.Common.nixpkgsInput,
    nixArgs,
    currentSystem,
  )
where

import Cradle (StdoutUntrimmed (..), run)
import Data.Aeson (eitherDecode)
import Data.String.Conversions (cs)
import Garn.ImportVersion (Versions (..), versionsSplice)

versionFromJson :: Versions
versionFromJson = $(versionsSplice)

garnCliVersion :: String
garnCliVersion = tsLibVersion versionFromJson

nixpkgsInput :: String
nixpkgsInput = Garn.ImportVersion.nixpkgsInput versionFromJson

nixArgs :: [String]
nixArgs =
  [ "--extra-experimental-features",
    "flakes nix-command",
    "--print-build-logs",
    "--include-untracked-files",
    "-vvv"
  ]

currentSystem :: IO String
currentSystem = do
  StdoutUntrimmed json <- run "nix" nixArgs (words "eval --impure --json --expr builtins.currentSystem")
  pure $ either error id $ eitherDecode (cs json)

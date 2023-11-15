{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Garn.ImportVersion (Versions (..), versionsSplice) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift (..))

data Versions = Versions
  { tsLibVersion :: String,
    nixpkgsInput :: String
  }
  deriving stock (Generic, Lift)
  deriving anyclass (FromJSON)

versionsSplice :: Q Exp
versionsSplice = do
  versions <- runIO $ eitherDecodeFileStrict' "./ts/internal/version.json"
  case versions of
    Right (versions :: Versions) -> [|versions|]
    Left err -> fail err

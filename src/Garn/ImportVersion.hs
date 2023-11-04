module Garn.ImportVersion (garnVersionSplice) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp (LitE), Lit (StringL), Q, runIO)

newtype Version = Version
  { tsLibVersion :: String
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

garnVersionSplice :: Q Exp
garnVersionSplice = do
  version <- runIO $ eitherDecodeFileStrict' "./ts/internal/version.json"
  case version of
    Right version -> pure $ LitE $ StringL $ tsLibVersion version
    Left err -> fail err

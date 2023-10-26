module Garn.Utils where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.String.Conversions (cs)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp (LitE), Lit (StringL), Q, runIO)
import Text.Pretty.Simple (pShow)

dbg :: (Show a) => a -> a
dbg a = trace (cs $ pShow a) a

newtype Version = Version
  { tsLibVersion :: String
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

garnVersion :: Q Exp
garnVersion = do
  version <- runIO $ eitherDecodeFileStrict' "./ts/internal/version.json"
  case version of
    Right version -> pure $ LitE $ StringL $ tsLibVersion version
    Left err -> fail err

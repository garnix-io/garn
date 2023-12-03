{-# LANGUAGE QuasiQuotes #-}

module NixOSCodeGenSpec
  ( spec,
  )
where

import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Garn.NixOSCodeGen
import Test.Hspec

gs :: IO [Service]
gs = getServices

spec :: Spec
spec = do
  fdescribe "generateTS" $ do
    it "generates a Service class with the appropriate setters" $
      do
        generateTS
          [ Service
              { name = "blah",
                options =
                  Option $
                    Map.fromList
                      [ ("enable", SimpleOption "bool" "enable?"),
                        ("foo", SimpleOption "int" "how many foo")
                      ]
              }
          ]
        `shouldBe` unindent
          [i|
        class Services {
            private config: Record<string, Object>;

            constructor() {
                this.config = {}
            }

            set blah(args: {enabled: boolean}) {
                this.config.blah = args;
            }
        }
      |]

  fdescribe "getServices" $ do
    it "returns known services" $ do
      result <- find (\x -> name x == "nginx") <$> gs
      result `shouldSatisfy` isJust
    it "includes simple options" $ do
      result <- find (\x -> name x == "postgresql") <$> gs
      (result >>= Map.lookup "enable" . getOption . options)
        `shouldBe` Just
          ( SimpleOption
              { optionType = "bool",
                description = "Whether to enable PostgreSQL Server."
              }
          )

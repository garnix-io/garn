{-# LANGUAGE QuasiQuotes #-}

module Garner.CodeGenSpec (spec) where

import Data.String.Interpolate (i)
import Garner.CodeGen
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

spec :: Spec
spec = do
  describe "fromToplevelDerivation" $ do
    it "extracts top-level derivations" $ do
      output <-
        fromToplevelDerivation
          "pkgs"
          [i|
            {
              foo = derivation {
                system = "x86_64-linux";
                name = "fooderivation";
                builder = "foo";
              };
            }
          |]
      return $ defaultGolden "hello" output
    it "ignores things that are not derivations" pending
    it "only generates valid typescript" pending
    it "generates nice JSDoc comments" pending

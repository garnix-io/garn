{-# LANGUAGE QuasiQuotes #-}

module Garner.CodeGenSpec (spec) where

import Data.String.Interpolate (i)
import Garner.CodeGen
import Test.Hspec
import Test.Hspec.Golden (Golden, defaultGolden)

spec :: Spec
spec = do
  describe "fromToplevelDerivation" $ do
    golden "extracts top-level derivations" $ \check -> do
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
      return $ check output
    it "ignores things that are not derivations" pending
    it "only generates valid typescript" pending
    it "generates nice JSDoc comments" pending

golden :: Example a => String -> ((String -> Golden String) -> a) -> SpecWith (Arg a)
golden description test =
  it description $
    test (defaultGolden $ fmap cleanChar description)
  where
    cleanChar ' ' = '_'
    cleanChar '/' = '_'
    cleanChar '\'' = '_'
    cleanChar '"' = '_'
    cleanChar x = x

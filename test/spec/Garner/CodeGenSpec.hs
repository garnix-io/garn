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
      check
        <$> fromToplevelDerivation
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
    golden "ignores things that are not derivations" $ \check -> do
      check
        <$> fromToplevelDerivation
          "pkgs"
          [i|
            {
              foo = derivation {
                system = "x86_64-linux";
                name = "fooderivation";
                builder = "foo";
              };
              bar = 3;
              baz = { isay = "hey"; };
            }
          |]
    golden "ignores attributes that throw" $ \check -> do
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
              throwing = throw "test error message";
            }
          |]
      return $ check output
    golden "ignores attributes that are marked broken" $ \check -> do
      check
        <$> fromToplevelDerivation
          "pkgs"
          [i|
            {
              broken = derivation {
                system = "x86_64-linux";
                name = "broken";
                builder = "foo";
              } // { meta.broken = true; };
              broken2 = derivation {
                system = "x86_64-linux";
                name = "broken2";
                builder = "foo";
              } // { meta.broken = throw "broken!"; };
              foo = derivation {
                system = "x86_64-linux";
                name = "foo";
                builder = "foo";
              } // { meta.broken = false; };
            }
          |]
    it "only generates valid typescript" pending
    golden "generates nice JSDoc comments" $ \check -> do
      check
        <$> fromToplevelDerivation
          "pkgs"
          [i|
            {
              foo = derivation {
                system = "x86_64-linux";
                name = "fooderivation";
                builder = "foo";
              } // { meta.description = "This is the bestest derivation."; };
            }
          |]

golden :: (Example a) => String -> ((String -> Golden String) -> a) -> SpecWith (Arg a)
golden description test =
  it description $
    test (defaultGolden $ fmap cleanChar description)
  where
    cleanChar ' ' = '_'
    cleanChar '/' = '_'
    cleanChar '\'' = '_'
    cleanChar '"' = '_'
    cleanChar x = x

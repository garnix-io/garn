{-# LANGUAGE QuasiQuotes #-}

module Garner.CodeGenSpec (spec) where

import Data.String.Interpolate (i)
import Development.Shake (cmd_)
import Garner.CodeGen
import System.Directory (getCurrentDirectory)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory

spec :: Spec
spec = do
  describe "fromToplevelDerivation" $ do
    checkTs "extracts top-level derivations" $
      [i|
        {
          foo = derivation {
            system = "x86_64-linux";
            name = "fooderivation";
            builder = "foo";
          };
        }
      |]
    checkTs "ignores things that are not derivations" $
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
    checkTs "ignores attributes that throw" $
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
    checkTs "ignores attributes that are marked broken" $
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
    checkTs "generates nice JSDoc comments" $
      [i|
        {
          foo = derivation {
            system = "x86_64-linux";
            name = "fooderivation";
            builder = "foo";
          } // { meta.description = "This is the bestest derivation."; };
        }
      |]

checkTs :: String -> String -> SpecWith ()
checkTs description nixExpression = do
  it (description <> " (golden)") $ do
    code <- fromToplevelDerivation "../../ts" "pkgs" nixExpression
    return $ defaultGolden cleanDescription code
  it (description <> " (typecheck)") $ do
    repoDir <- getCurrentDirectory
    code <- fromToplevelDerivation (repoDir <> "/ts") "pkgs" nixExpression
    inTempDirectory $ do
      let file = cleanDescription <> ".ts"
      writeFile file code
      cmd_ "deno check --quiet" file
  where
    cleanDescription = fmap cleanChar description
    cleanChar ' ' = '_'
    cleanChar '/' = '_'
    cleanChar '\'' = '_'
    cleanChar '"' = '_'
    cleanChar x = x

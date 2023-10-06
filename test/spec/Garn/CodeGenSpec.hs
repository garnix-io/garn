{-# LANGUAGE QuasiQuotes #-}

module Garn.CodeGenSpec (spec) where

import Data.String.Interpolate (i)
import Development.Shake (cmd_)
import Garn.CodeGen
import System.Directory (getCurrentDirectory)
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory
import Test.Mockery.Environment (withModifiedEnvironment)

spec :: Spec
spec = do
  around_ (withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]) $ do
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
      checkTs "generates nice JSDoc comments" $
        [i|
          {
            foo = derivation {
              system = "x86_64-linux";
              name = "fooderivation";
              builder = "foo";
            } // { meta.description = "This is the bestest derivation."; };

            bar = derivation {
              system = "x86_64-linux";
              name = "fooderivation";
              builder = "foo";
            } // { meta.description = "This one - eh, not so good!"; };
          }
        |]
      checkTs "sanitizes package names" $
        [i|
          {
            foo-a = derivation {
              system = "x86_64-linux";
              name = "fooderivation";
              builder = "foo";
            };
            catch = derivation {
              system = "x86_64-linux";
              name = "keyword";
              builder = "foo";
            };
            super = derivation {
              system = "x86_64-linux";
              name = "keyword";
              builder = "foo";
            };
            void = derivation {
              system = "x86_64-linux";
              name = "keyword";
              builder = "foo";
            };
            instanceOf = derivation {
              system = "x86_64-linux";
              name = "keyword";
              builder = "foo";
            };
          }
        |]
      checkTs "removes conflicts due to sanitization" $
        [i|
          {
            foo-a = derivation {
              system = "x86_64-linux";
              name = "fooderivation1";
              builder = "foo";
            };
            foo_a = derivation {
              system = "x86_64-linux";
              name = "fooderivation2";
              builder = "foo";
            };
          }
        |]
      checkTs "adds a description attribute from the nix package" $
        [i|
          {
            foo-a = derivation {
              system = "x86_64-linux";
              name = "fooderivation1";
              meta.description = "some description";
              builder = "foo";
            };
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

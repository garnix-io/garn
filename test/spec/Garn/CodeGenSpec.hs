{-# LANGUAGE QuasiQuotes #-}

module Garn.CodeGenSpec (spec) where

import Data.Map (Map, singleton)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Garn.CodeGen (PkgInfo (Collection, Derivation, description, path, subPkgs), scanPackages, writePkgFiles)
import Garn.Common (currentSystem)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Mockery.Environment (withModifiedEnvironment)

inParallel :: Spec -> Spec
inParallel spec = do
  runIO $ do
    n <- getNumProcessors
    setNumCapabilities (max 1 (n - 1))
  parallel spec

spec :: Spec
spec = inParallel $ do
  describe "scanPackages" $ do
    testScanPackages
      "empty pkgSpec collects all top level derivations, but skips nested ones"
      [i|
        {
          foo = derivation {
            system = "x86_64-linux";
            name = "fooderivation";
            builder = "foo";
          };
          bar = derivation {
            system = "x86_64-linux";
            name = "barderivation";
            builder = "bar";
          };
          nested = {
            baz = derivation {
              system = "x86_64-linux";
              name = "bazderivation";
              builder = "baz";
            };
          };
        }
      |]
      [i|
        collectionSpec {}
      |]
      $ ("foo" ~> Derivation {description = Nothing, path = "pkgs.foo"})
        <> ("bar" ~> Derivation {description = Nothing, path = "pkgs.bar"})

    testScanPackages
      "finds nested derivations specified by pkgSpec"
      [i|
        {
          foo = derivation {
            system = "x86_64-linux";
            name = "fooderivation";
            builder = "foo";
          };
          nested = {
            nested-once = derivation {
              system = "x86_64-linux";
              name = "nested-once-derivation";
              builder = "nested-once";
            };
            nested-again = {
              nested-twice = derivation {
                system = "x86_64-linux";
                name = "nested-twice-derivation";
                builder = "nested-twice";
              };
            };
          };
        }
      |]
      [i|
        collectionSpec {
          nested = collectionSpec {};
        }
      |]
      $ ("foo" ~> Derivation {description = Nothing, path = "pkgs.foo"})
        <> ( "nested"
               ~> Collection
                 { subPkgs = "nested-once" ~> Derivation {description = Nothing, path = "pkgs.nested.nested-once"}
                 }
           )

    testScanPackages
      "removes derivations omitted by the pkgSpec"
      [i|
        {
          good = derivation {
            system = "x86_64-linux";
            name = "fooderivation";
            builder = "good";
          };
          bad = derivation {
            system = "x86_64-linux";
            name = "fooderivation";
            builder = "bad";
          };
          nested = {
            nested-again = {
              nested-good = derivation {
                system = "x86_64-linux";
                name = "nested-good-derivation";
                builder = "nested-good";
              };
              nested-bad = derivation {
                system = "x86_64-linux";
                name = "nested-bad-derivation";
                builder = "nested-bad";
              };
            };
          };
        }
      |]
      [i|
        collectionSpec {
          bad = false;
          nested = collectionSpec {
            nested-again = collectionSpec {
              nested-bad = false;
            };
          };
        }
      |]
      $ ("good" ~> Derivation {description = Nothing, path = "pkgs.good"})
        <> ( "nested"
               ~> Collection
                 { subPkgs =
                     "nested-again" ~> Collection {subPkgs = "nested-good" ~> Derivation {description = Nothing, path = "pkgs.nested.nested-again.nested-good"}}
                 }
           )

    testScanPackages
      "ignores things that are not derivations"
      [i|
        {
          foo = derivation {
            system = "x86_64-linux";
            name = "fooderivation";
            builder = "foo";
          };
          bar = 3;
          baz = { isa = "hey"; };
        }
      |]
      [i|
        collectionSpec {}
      |]
      ("foo" ~> Derivation {description = Nothing, path = "pkgs.foo"})

    testScanPackages
      "ignores attributes that throw"
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
      [i|
        collectionSpec {}
      |]
      ("foo" ~> Derivation {description = Nothing, path = "pkgs.foo"})

    testScanPackages
      "ignores attributes that are marked broken"
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
      [i|
        collectionSpec {}
      |]
      ("foo" ~> Derivation {description = Nothing, path = "pkgs.foo"})

    testScanPackages
      "outputs package descriptions to be turned into JSDoc comments"
      [i|
        {
          foo = derivation {
            system = "x86_64-linux";
            name = "fooderivation";
            builder = "foo";
            meta.description = "This is the bestest derivation.";
          };
          bar = derivation {
            system = "x86_64-linux";
            name = "fooderivation";
            builder = "foo";
             meta.description = "This one - eh, not so good!";
          };
        }
      |]
      [i|
        collectionSpec {}
      |]
      $ ("foo" ~> Derivation {description = Just "This is the bestest derivation.", path = "pkgs.foo"})
        <> ("bar" ~> Derivation {description = Just "This one - eh, not so good!", path = "pkgs.bar"})

  describe "writePkgFiles" $ do
    testWritePkgFiles
      "serializes derivations to garn packages"
      ("foo" ~> Derivation {description = Nothing, path = "pkgs.foo"})
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "path/to/garn/root/package.ts";
              import { nixRaw } from "path/to/garn/root/nix.ts";

              export const foo = mkPackage(
                nixRaw`pkgs.foo`,
                "",
              );
            |]

    testWritePkgFiles
      "serializes package descriptions"
      ("foo" ~> Derivation {description = Just "Description for foo package", path = "pkgs.foo"})
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "path/to/garn/root/package.ts";
              import { nixRaw } from "path/to/garn/root/nix.ts";

              /**
               * Description for foo package
               */
              export const foo = mkPackage(
                nixRaw`pkgs.foo`,
                "Description for foo package",
              );
            |]

    testWritePkgFiles
      "serializes nested derivations into separate files"
      ( ("nested-1" ~> Collection {subPkgs = "nested-2" ~> Collection {subPkgs = "some-pkg" ~> Derivation {description = Nothing, path = "pkgs.nested-1.nested-2.some-pkg"}}})
          <> ("foo" ~> Derivation {description = Nothing, path = "pkgs.foo"})
      )
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "path/to/garn/root/package.ts";
              import { nixRaw } from "path/to/garn/root/nix.ts";

              export const foo = mkPackage(
                nixRaw`pkgs.foo`,
                "",
              );

              export * as nested_1 from "./nested_1/mod.ts";
            |]
        readFile "nested_1/mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "path/to/garn/root/../package.ts";
              import { nixRaw } from "path/to/garn/root/../nix.ts";

              export * as nested_2 from "./nested_2/mod.ts";
            |]
        readFile "nested_1/nested_2/mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "path/to/garn/root/../../package.ts";
              import { nixRaw } from "path/to/garn/root/../../nix.ts";

              export const some_pkg = mkPackage(
                nixRaw`pkgs.nested-1.nested-2.some-pkg`,
                "",
              );
            |]

    testWritePkgFiles
      "sanitizes package names"
      ( ("a-package" ~> Derivation {description = Nothing, path = "pkgs.a-package"})
          <> ("a/b@c" ~> Derivation {description = Nothing, path = "pkgs.a/b@c"})
          <> ("catch" ~> Derivation {description = Nothing, path = "pkgs.catch"})
          <> ("instanceOf" ~> Derivation {description = Nothing, path = "pkgs.instanceOf"})
          <> ("super" ~> Derivation {description = Nothing, path = "pkgs.super"})
          <> ("void" ~> Derivation {description = Nothing, path = "pkgs.void"})
      )
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "path/to/garn/root/package.ts";
              import { nixRaw } from "path/to/garn/root/nix.ts";

              export const a_b_c = mkPackage(
                nixRaw`pkgs.a/b@c`,
                "",
              );

              export const a_package = mkPackage(
                nixRaw`pkgs.a-package`,
                "",
              );

              export const catch_ = mkPackage(
                nixRaw`pkgs.catch`,
                "",
              );

              export const instanceOf_ = mkPackage(
                nixRaw`pkgs.instanceOf`,
                "",
              );

              export const super_ = mkPackage(
                nixRaw`pkgs.super`,
                "",
              );

              export const void_ = mkPackage(
                nixRaw`pkgs.void`,
                "",
              );
            |]
    testWritePkgFiles
      "removes conflicts due to sanitization"
      ( ("a-package" ~> Derivation {description = Nothing, path = "pkgs.a-package"})
          <> ("a_package" ~> Derivation {description = Nothing, path = "pkgs.a_package"})
          <> ("a/package" ~> Derivation {description = Nothing, path = "pkgs.a/package"})
      )
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "path/to/garn/root/package.ts";
              import { nixRaw } from "path/to/garn/root/nix.ts";

              export const a_package = mkPackage(
                nixRaw`pkgs.a_package`,
                "",
              );
            |]

testScanPackages :: String -> String -> String -> Map String PkgInfo -> SpecWith ()
testScanPackages description pkgs pkgSpec expectedOutput =
  it description $ do
    withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")] $ do
      system <- currentSystem
      actualOutput <- scanPackages system pkgs pkgSpec
      actualOutput `shouldBe` expectedOutput

testWritePkgFiles :: String -> Map String PkgInfo -> ((FilePath -> IO String) -> IO ()) -> SpecWith ()
testWritePkgFiles description pkgInfo runTest =
  it description $ do
    withSystemTempDirectory "writePkgFiles-test" $ \tempDir -> do
      writePkgFiles tempDir "path/to/garn/root" pkgInfo
      runTest $ \path -> readFile $ tempDir </> path

(~>) :: key -> value -> Map key value
(~>) = singleton

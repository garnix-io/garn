{-# LANGUAGE QuasiQuotes #-}

module Garn.CodeGenSpec (spec) where

import Data.Map (Map, singleton)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Garn.CodeGen (PkgInfo (Collection, Derivation, description, path), scanPackages, writePkgFiles)
import Garn.Common (currentSystem)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Mockery.Environment (withModifiedEnvironment)

spec :: Spec
spec = do
  describe "scanPackages" $ do
    it "empty pkgSpec collects all top level derivations, but skips nested ones"
      $ testScanPackages
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
        "{}"
      $ mempty
        <> "foo" ~> Derivation {description = Nothing, path = "pkgs.foo"}
        <> "bar" ~> Derivation {description = Nothing, path = "pkgs.bar"}

    it "finds nested derivations specified by pkgSpec"
      $ testScanPackages
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
        "{ nested = {}; }"
      $ mempty
        <> "foo" ~> Derivation {description = Nothing, path = "pkgs.foo"}
        <> "nested" ~> Collection ("nested-once" ~> Derivation {description = Nothing, path = "pkgs.nested.nested-once"})

    it "removes derivations omitted by the pkgSpec"
      $ testScanPackages
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
          {
            bad = false;
            nested = {
              nested-again = {
                nested-bad = false;
              };
            };
          }
        |]
      $ mempty
        <> "good" ~> Derivation {description = Nothing, path = "pkgs.good"}
        <> "nested"
          ~> Collection
            ( "nested-again"
                ~> Collection
                  ( "nested-good" ~> Derivation {description = Nothing, path = "pkgs.nested.nested-again.nested-good"}
                  )
            )

    it "ignores things that are not derivations"
      $ testScanPackages
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
        "{}"
      $ "foo" ~> Derivation {description = Nothing, path = "pkgs.foo"}

    it "ignores attributes that throw"
      $ testScanPackages
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
        "{}"
      $ "foo" ~> Derivation {description = Nothing, path = "pkgs.foo"}

    it "ignores attributes that are marked broken"
      $ testScanPackages
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
        "{}"
      $ "foo" ~> Derivation {description = Nothing, path = "pkgs.foo"}

    it "outputs package descriptions to be turned into JSDoc comments"
      $ testScanPackages
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
        "{}"
      $ mempty
        <> "foo" ~> Derivation {description = Just "This is the bestest derivation.", path = "pkgs.foo"}
        <> "bar" ~> Derivation {description = Just "This one - eh, not so good!", path = "pkgs.bar"}

  describe "writePkgFiles" $ do
    it "serializes derivations to garn packages"
      $ testWritePkgFiles
        ("foo" ~> Derivation {description = Nothing, path = "pkgs.foo"})
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "test-garn-import-root/package.ts";
              import { nixRaw } from "test-garn-import-root/nix.ts";

              export const foo = mkPackage(
                nixRaw`pkgs.foo`,
                "",
              );
            |]

    it "serializes package descriptions"
      $ testWritePkgFiles
        ("foo" ~> Derivation {description = Just "Description for foo package", path = "pkgs.foo"})
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "test-garn-import-root/package.ts";
              import { nixRaw } from "test-garn-import-root/nix.ts";

              /**
               * Description for foo package
               */
              export const foo = mkPackage(
                nixRaw`pkgs.foo`,
                "Description for foo package",
              );
            |]

    it "serializes nested derivations into separate files"
      $ testWritePkgFiles
        ( mempty
            <> "foo" ~> Derivation {description = Nothing, path = "pkgs.foo"}
            <> "nested-1"
              ~> Collection
                ( "nested-2"
                    ~> Collection
                      ( "some-pkg" ~> Derivation {description = Nothing, path = "pkgs.nested-1.nested-2.some-pkg"}
                      )
                )
        )
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "test-garn-import-root/package.ts";
              import { nixRaw } from "test-garn-import-root/nix.ts";

              export const foo = mkPackage(
                nixRaw`pkgs.foo`,
                "",
              );

              export * as nested_1 from "./nested_1/mod.ts";
            |]
        readFile "nested_1/mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "test-garn-import-root/../package.ts";
              import { nixRaw } from "test-garn-import-root/../nix.ts";

              export * as nested_2 from "./nested_2/mod.ts";
            |]
        readFile "nested_1/nested_2/mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "test-garn-import-root/../../package.ts";
              import { nixRaw } from "test-garn-import-root/../../nix.ts";

              export const some_pkg = mkPackage(
                nixRaw`pkgs.nested-1.nested-2.some-pkg`,
                "",
              );
            |]

    it "sanitizes package names"
      $ testWritePkgFiles
        ( mempty
            <> "a-package" ~> Derivation {description = Nothing, path = "pkgs.a-package"}
            <> "a/b@c" ~> Derivation {description = Nothing, path = "pkgs.a/b@c"}
            <> "catch" ~> Derivation {description = Nothing, path = "pkgs.catch"}
            <> "instanceOf" ~> Derivation {description = Nothing, path = "pkgs.instanceOf"}
            <> "super" ~> Derivation {description = Nothing, path = "pkgs.super"}
            <> "void" ~> Derivation {description = Nothing, path = "pkgs.void"}
        )
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "test-garn-import-root/package.ts";
              import { nixRaw } from "test-garn-import-root/nix.ts";

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

    it "removes conflicts due to sanitization"
      $ testWritePkgFiles
        ( mempty
            <> "a-package" ~> Derivation {description = Nothing, path = "pkgs.a-package"}
            <> "a_package" ~> Derivation {description = Nothing, path = "pkgs.a_package"}
            <> "a/package" ~> Derivation {description = Nothing, path = "pkgs.a/package"}
        )
      $ \readFile -> do
        readFile "mod.ts"
          `shouldReturn` unindent
            [i|
              import { mkPackage } from "test-garn-import-root/package.ts";
              import { nixRaw } from "test-garn-import-root/nix.ts";

              export const a_package = mkPackage(
                nixRaw`pkgs.a_package`,
                "",
              );
            |]

testScanPackages :: String -> String -> Map String PkgInfo -> IO ()
testScanPackages pkgs pkgSpec expectedOutput =
  withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")] $ do
    system <- currentSystem
    actualOutput <- scanPackages system pkgs pkgSpec
    actualOutput `shouldBe` expectedOutput

testWritePkgFiles :: Map String PkgInfo -> ((FilePath -> IO String) -> IO ()) -> IO ()
testWritePkgFiles pkgInfo runTest =
  withSystemTempDirectory "writePkgFiles-test" $ \tempDir -> do
    writePkgFiles tempDir "test-garn-import-root" pkgInfo
    runTest $ \path -> readFile $ tempDir </> path

(~>) :: key -> value -> Map key value
(~>) = singleton

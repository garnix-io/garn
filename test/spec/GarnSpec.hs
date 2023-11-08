{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GarnSpec (spec) where

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import System.Directory
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory (inTempDirectory)
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils

wrap :: SpecWith (ProcResult -> IO ()) -> Spec
wrap =
  around_ (withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")])
    . around onTestFailureLogger

spec :: Spec
spec = do
  repoDir <- runIO getCurrentDirectory

  wrap $ do
    describe "garn" $ around_ inTempDirectory $ do
      describe "--help" $ do
        it "lists available commands" $ \onTestFailureLog -> do
          output <- runGarn ["--help"] "" repoDir Nothing
          onTestFailureLog output
          stdout output
            `shouldMatch` unindent
              [i|
                Available commands:
                  init.*
              |]
          writeFile "garn.ts" [i|import "#{repoDir}/ts/mod.ts"|]
          output <- runGarn ["--help"] "" repoDir Nothing
          onTestFailureLog output
          stdout output
            `shouldMatch` unindent
              [i|
                Available commands:
                  build.*
                  run.*
                  enter.*
                  generate.*
                  check.*
              |]
        it "lists unavailable commands" $ \onTestFailureLog -> do
          output <- runGarn ["--help"] "" repoDir Nothing
          onTestFailureLog output
          stdout output
            `shouldMatch` unindent
              [i|
                Unavailable commands:
                  build
                  run
                  enter
                  generate
                  check
              |]
          writeFile "garn.ts" [i|import "#{repoDir}/ts/mod.ts"|]
          output <- runGarn ["--help"] "" repoDir Nothing
          onTestFailureLog output
          stdout output
            `shouldMatch` unindent
              [i|
                Unavailable commands:
                  init
              |]

      it "generates formatted flakes" $ \onTestFailureLog -> do
        inTempDirectory $ do
          writeHaskellProject repoDir
          output <- runGarn ["run", "foo"] "" repoDir Nothing
          onTestFailureLog output
          flake <- readFile "./flake.nix"
          pure $ defaultGolden "generates_formatted_flakes" flake

      it "outputs a version with --version" $ \onTestFailureLog -> do
        output <- runGarn ["--version"] "" repoDir Nothing
        onTestFailureLog output
        stdout output `shouldBe` "v0.0.15\n"
        stderr output `shouldBe` ""
        exitCode output `shouldBe` ExitSuccess

      describe "version mismatches" $ do
        it "gives a helpful error messages when there's a deno <-> haskell json version mismatch" $
          \onTestFailureLog -> do
            inTempDirectory $ do
              writeFile
                "garn.ts"
                [i|
                  type DenoOutput = {
                    "some incompatible type": null,
                    garnTsLibVersion: string
                  };

                  function toDenoOutput(
                    nixpkgsInput: string,
                    garnExports: Record<string, unknown>,
                  ): DenoOutput {
                    return {
                      "some incompatible type": null,
                      garnTsLibVersion: "test version string",
                    };
                  }

                  type InternalLibrary = {
                    toDenoOutput: (
                      nixpkgsInput: string,
                      garnExports: Record<string, unknown>,
                    ) => DenoOutput;
                  };

                  const garnGetInternalLib = (): InternalLibrary => ({
                    toDenoOutput,
                  });

                  // deno-lint-ignore no-explicit-any
                  if ((window as any).__garnGetInternalLib != null) {
                    throw new Error(
                      "Registering __garnGetInternalLib twice, using two different garn library versions is not supported.",
                    );
                  }
                  // deno-lint-ignore no-explicit-any
                  (window as any).__garnGetInternalLib = garnGetInternalLib;
                |]
              output <- runGarn ["run", "whatever"] "" repoDir Nothing
              onTestFailureLog output
              stderr output
                `shouldBe` unindent
                  [i|
                    [garn] Error: Version mismatch detected:
                    'garn' (the cli tool) is not compatible with the version of the garn typescript library you're using.
                    Try installing version `test version string` of 'garn' (the cli tool).
                    (Internal details: Error in $: key \"tag\" not found)
                  |]

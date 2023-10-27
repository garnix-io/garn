{-# LANGUAGE QuasiQuotes #-}

module InitSpec where

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import System.Directory
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment
import TestUtils

spec :: Spec
spec = do
  describe "init" $ do
    repoDir <- runIO getCurrentDirectory
    around_
      ( withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]
          . inTempDirectory
      )
      $ do
        it "can initialize haskell projects" $ do
          writeFile
            "garn.cabal"
            [i|
              name: garn
              version: 0.0.1
            |]
          output <- runGarn ["init"] "" repoDir Nothing
          stderr output `shouldBe` "[garn] Creating a garn.ts file\n"
          readFile "garn.ts"
            `shouldReturn` unindent
              [i|
                import * as garn from "https://garn.io/ts/v0.0.13/mod.ts";
                import * as pkgs from "https://garn.io/ts/v0.0.13/nixpkgs.ts";

                export const garn = garn.haskell.mkHaskellProject({
                  description: "",
                  executable: "",
                  compiler: "ghc94",
                  src: "."
                })
              |]
        it "can initialize go projects" $ do
          writeFile
            "go.mod"
            ( unindent
                [i|
                  module github.com/garnix-io/some-go-project
                  go 1.20
                |]
            )
          output <- runGarn ["init"] "" repoDir Nothing
          stderr output `shouldBe` "[garn] Creating a garn.ts file\n"
          readFile "garn.ts"
            `shouldReturn` unindent
              [i|
                import * as garn from "https://garn.io/ts/v0.0.13/mod.ts";
                import * as pkgs from "https://garn.io/ts/v0.0.13/nixpkgs.ts";

                export const someGoProject = garn.go.mkGoProject({
                  description: "My go project",
                  src: ".",
                  goVersion: "1.20",
                });
              |]
        it "logs unexpected errors" $ do
          writeFile "garn.cabal" [i| badCabalfile |]
          output <- runGarn ["init"] "" repoDir Nothing
          stderr output
            `shouldBe` unindent
              [i|
                [garn] Creating a garn.ts file
                [garn] Found but could not parse cabal file
              |]

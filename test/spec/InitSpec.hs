{-# LANGUAGE QuasiQuotes #-}

module InitSpec where

import Control.Monad (forM_, when)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Development.Shake (StdoutTrim (..), cmd, cmd_)
import System.Directory
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment
import TestUtils

wrapTest :: SpecWith (ProcResult -> IO ()) -> Spec
wrapTest =
  aroundAll_ withFileServer
    . around_
      ( withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]
          . inTempDirectory
      )
    . around onTestFailureLogger

spec :: Spec
spec = do
  describe "init" $ do
    repoDir <- runIO getCurrentDirectory
    wrapTest $ do
      it "can initialize haskell projects" $ \onTestFailureLog -> do
        writeFile
          "test.cabal"
          [i|
            name: test
            version: 0.0.1
            executable test
              main-is: Main.hs
              build-depends: base
              default-language: Haskell2010
          |]
        writeFile
          "Main.hs"
          [i|
            main = putStrLn "hello world"
          |]
        output <- runGarn ["init"] "" repoDir Nothing
        onTestFailureLog output
        stderr output `shouldBe` "[garn] Creating a garn.ts file\n"
        readFile "garn.ts"
          `shouldReturn` unindent
            [i|
              import * as garn from "https://garn.io/ts/v0.0.15/mod.ts";
              import * as pkgs from "https://garn.io/ts/v0.0.15/nixpkgs.ts";

              export const test = garn.haskell.mkHaskellProject({
                description: "",
                executable: "",
                compiler: "ghc94",
                src: "."
              })
            |]
        rewriteImportsToLocalhost
        output <- runGarn ["build", "test"] "" repoDir Nothing
        onTestFailureLog output
        (StdoutTrim output) <- cmd "./result/bin/test"
        output `shouldBe` "hello world"

      it "can initialize go projects" $ \onTestFailureLog -> do
        writeFile
          "go.mod"
          $ unindent
            [i|
              module github.com/garnix-io/some-go-project
              go 1.21
            |]
        writeFile "main.go" $
          unindent
            [i|
              package main

              import "fmt"

              func main() {
                fmt.Println("hello world")
              }
            |]
        output <- runGarn ["init"] "" repoDir Nothing
        onTestFailureLog output
        stderr output `shouldBe` "[garn] Creating a garn.ts file\n"
        readFile "garn.ts"
          `shouldReturn` unindent
            [i|
              import * as garn from "https://garn.io/ts/v0.0.15/mod.ts";
              import * as pkgs from "https://garn.io/ts/v0.0.15/nixpkgs.ts";

              export const someGoProject = garn.go.mkGoProject({
                description: "My go project",
                src: ".",
                goVersion: "1.21",
              });
            |]
        rewriteImportsToLocalhost
        output <- runGarn ["build", "someGoProject"] "" repoDir Nothing
        onTestFailureLog output
        (StdoutTrim output) <- cmd "./result/bin/some-go-project"
        output `shouldBe` "hello world"

      it "can initialize npm projects" $ \onTestFailureLog -> do
        writeFile
          "package.json"
          [i|
            {
              "name": "my-project",
              "scripts": {
                "start": "echo starting my project...",
                "test": "echo testing..."
              }
            }
          |]
        output <- runGarn ["init"] "" repoDir Nothing
        onTestFailureLog output
        stderr output `shouldBe` "[garn] Creating a garn.ts file\n"
        rewriteImportsToLocalhost
        output <- runGarn ["run", "myProject.start"] "" repoDir Nothing
        onTestFailureLog output
        stdout output `shouldContain` "starting my project..."

      it "adds a builder Package to vite projects" $ \onTestFailureLog -> do
        inExampleCopy repoDir "vite-frontend" $ do
          forM_ ["garn.ts", "flake.nix", "flake.lock"] removeFile
          output <- runGarn ["init"] "" repoDir Nothing
          onTestFailureLog output
          rewriteImportsToLocalhost
          output <- runGarn ["build", "viteFrontend.build"] "" repoDir Nothing
          onTestFailureLog output
          indexFile <- readFile "result/index.html"
          indexFile `shouldContain` "<title>Vite + TS</title>"

      it "logs unexpected errors" $ \onTestFailureLog -> do
        writeFile "garn.cabal" [i| badCabalfile |]
        output <- runGarn ["init"] "" repoDir Nothing
        onTestFailureLog output
        stderr output
          `shouldBe` unindent
            [i|
              [garn] Found but could not parse cabal file
              [garn] Cannot detect any project toolchains, sorry! Creating example garn.ts file
            |]

      describe "when no specific initializer runs" $ do
        it "prints out a message about that" $ \onTestFailureLog -> do
          output <- runGarn ["init"] "" repoDir Nothing
          onTestFailureLog output
          stderr output
            `shouldBe` unindent
              [i|
                [garn] Cannot detect any project toolchains, sorry! Creating example garn.ts file
              |]

        it "generates a commented file, except for imports" $ \onTestFailureLog -> do
          output <- runGarn ["init"] "" repoDir Nothing
          onTestFailureLog output
          garnFile <- lines <$> readFile "garn.ts"
          take 2 garnFile
            `shouldBe` [ "import * as garn from \"https://garn.io/ts/v0.0.15/mod.ts\";",
                         "import * as pkgs from \"https://garn.io/ts/v0.0.15/nixpkgs.ts\";"
                       ]
          forM_ (drop 2 garnFile) $ \line ->
            when (line /= "") $
              line `shouldStartWith` "//"

rewriteImportsToLocalhost :: IO ()
rewriteImportsToLocalhost = do
  cmd_ "sd" "https://garn.io/ts/v[0-9]+\\.[0-9]+\\.[0-9]+/" "http://localhost:8777/" "garn.ts"

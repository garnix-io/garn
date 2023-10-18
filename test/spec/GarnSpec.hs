{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GarnSpec (spec) where

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import System.Directory
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory (inTempDirectory)
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils

spec :: Spec
spec = do
  repoDir <- runIO getCurrentDirectory

  around_ (withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]) $ do
    describe "garn" $ around_ inTempDirectory $ do
      describe "--help" $ do
        it "lists available commands" $ do
          output <- runGarn ["--help"] "" repoDir Nothing
          stdout output
            `shouldMatch` unindent
              [i|
                Available commands:
                  init.*
              |]
          writeFile "garn.ts" [i|import "#{repoDir}/ts/mod.ts"|]
          output <- runGarn ["--help"] "" repoDir Nothing
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
        it "lists unavailable commands" $ do
          output <- runGarn ["--help"] "" repoDir Nothing
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
          stdout output
            `shouldMatch` unindent
              [i|
                Unavailable commands:
                  init
              |]

    -- TODO: Golden tests currently can’t be integrated with the other test cases
    --       because stackbuilders/hspec-golden#40. The case below shows the
    --       effect that @`around_` `inTempDirectory`@ _should_ have.
    describe "garn-golden" $ do
      describe "run" $ do
        it "generates formatted flakes" $ do
          inTempDirectory $ do
            writeHaskellProject repoDir
            _ <- runGarn ["run", "foo"] "" repoDir Nothing
            flake <- readFile "./flake.nix"
            pure $ defaultGolden "generates_formatted_flakes" flake

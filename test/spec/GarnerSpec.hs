{-# LANGUAGE QuasiQuotes #-}

module GarnerSpec where

import Data.String.Interpolate (i)
import Development.Shake
import Garner
import System.Directory
import System.Environment (withArgs)
import System.IO.Silently (capture_)
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = do
  repoDir <- runIO getCurrentDirectory

  describe "garner" $ around_ inTempDirectory $ do
    it "runs a simple Haskell program" $ do
      writeFile
        "garner.ts"
        [i|
           import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

           export const foo = mkHaskell({
             compiler: "ghc94",
             src: "./."
           })
        |]
      writeFile
        "Main.hs"
        [i|
          main :: IO ()
          main = putStrLn "haskell test output"
        |]
      writeFile
        "package.yaml"
        [i|
          executables:
            garner-test:
              main: Main.hs
              dependencies:
               - base
        |]
      output <- capture_ $ withArgs ["run", "foo"] $ runWith
        (Options { tsRunnerFilename = repoDir <> "/ts/runner.ts"})
      output `shouldBe` "haskell test output\n"

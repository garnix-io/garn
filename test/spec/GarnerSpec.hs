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
           import "#{repoDir}/ts/haskell.ts"

           export const foo = mkHaskell({
             compiler: "ghc-9.4.2",
             src: "."
           })
        |]
      writeFile
        "Main.hs"
        [i|
          main :: IO ()
          main = putStrLn "foo"
        |]
      writeFile
        "package.yaml"
        [i|
          executables:
            garner-test:
              main: Main.hs
        |]
      output <- capture_ $ withArgs ["run", "foo"] run
      output `shouldBe` "foo"

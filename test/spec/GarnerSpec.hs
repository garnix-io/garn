{-# LANGUAGE QuasiQuotes #-}
module GarnerSpec where

import Garner
import Test.Mockery.Directory
import Test.Hspec (describe, Spec, it)

spec :: Spec
spec = around_ inTempDirectory $ do

  describe "garner" $ do

    it "runs a simple Haskell program" $ do
      writeFile "garner.ts" [i|
           export foo = mkHaskell({
             compiler: "ghc-9.4.2",
             src: "."
           })
      |]
      writeFile "Main.hs" [i|
        main :: IO ()
        main = putStrLn "foo"
      |]
      writeFile "package.yaml" [i|
        executables:
          garner-test:
            main: Main.hs
      |]
      run config `shouldReturn` "foo"

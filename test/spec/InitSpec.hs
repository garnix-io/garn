{-# LANGUAGE QuasiQuotes #-}

module InitSpec where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import System.Directory
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
  repoDir <- runIO getCurrentDirectory
  describe "init" $ do
    it "uses the provided init function if there is one" $ do
      writeFile
        "garn.cabal"
        [i|
              name: garn
              version: 0.0.1
            |]
      output <- runGarn ["init"] "" repoDir Nothing
      stderr output `shouldBe` "[garn] Creating a garn.ts file\n"
      readFile "garn.ts"
        `shouldReturn` dropWhileEnd
          isSpace
          ( unindent
              [i|
                    import * as garn from "http://localhost:8777/mod.ts"

                    export const garn = garn.haskell.mkHaskell({
                      description: "",
                      executable: "",
                      compiler: "ghc94",
                      src: "."
                    })
                  |]
          )
    it "logs unexpected errors" $ do
      writeFile "garn.cabal" [i| badCabalfile |]
      output <- runGarn ["init"] "" repoDir Nothing
      stderr output
        `shouldBe` unindent
          [i|
                [garn] Creating a garn.ts file
                [garn] Found but could not parse cabal file
              |]

{-# LANGUAGE QuasiQuotes #-}

module GarnerSpec where

import Data.String.Interpolate (i)
import GHC.IO.IOMode (IOMode (ReadMode))
import Garner
import System.Directory
import System.Environment (withArgs)
import System.IO (stdin, withFile)
import System.IO.Silently (capture_)
import Test.Hspec
import Test.Mockery.Directory (inTempDirectory)

spec :: Spec
spec = do
  repoDir <- runIO getCurrentDirectory

  describe "garner" $ around_ inTempDirectory $ do
    describe "run" $ do
      it "runs a simple Haskell program" $ do
        writeHaskellProject repoDir
        output <-
          capture_ $
            withArgs ["run", "foo"] $
              runWith
                (Options {stdin = System.IO.stdin, tsRunnerFilename = repoDir <> "/ts/runner.ts"})
        output `shouldBe` "haskell test output\n"
      it "writes flake files" pending
      it "doesn't write any other files" pending
    describe "enter" $ do
      it "has the right GHC version" $ do
        writeHaskellProject repoDir
        writeFile "stdin" "ghc --numeric-version\nexit\n"
        output <-
          capture_ $
            withFile "stdin" ReadMode $ \stdin ->
              withArgs ["enter", "foo"] $
                runWith (Options {stdin, tsRunnerFilename = repoDir <> "/ts/runner.ts"})
        output `shouldStartWith` "9.4"

writeHaskellProject :: FilePath -> IO ()
writeHaskellProject repoDir = do
  writeFile
    "garner.ts"
    [i|
      import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

      export const foo = mkHaskell({
        name: "mkHaskell-test",
        executable: "garner-test",
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

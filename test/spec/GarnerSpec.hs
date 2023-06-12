{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GarnerSpec where

import Control.Lens (from, (&), (<>~))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.List (sort)
import Data.String.Interpolate (i)
import Data.Vector.Generic.Lens (vector)
import qualified Data.Yaml as Yaml
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
        output <- runGarnerWithStdin ["run", "foo"] "" repoDir
        output `shouldBe` "haskell test output\n"
      it "writes flake.{lock,nix}, but no other files" $ do
        writeHaskellProject repoDir
        filesBefore <- listDirectory "."
        _ <- runGarnerWithStdin ["run", "foo"] "" repoDir
        filesAfter <- sort . filter (/= "stdin") <$> listDirectory "."
        filesAfter `shouldBe` sort (filesBefore ++ ["flake.lock", "flake.nix"])
    describe "enter" $ do
      it "has the right GHC version" $ do
        writeHaskellProject repoDir
        output <- runGarnerWithStdin ["enter", "foo"] "ghc --numeric-version\nexit\n" repoDir
        output `shouldStartWith` "9.4"
      it "registers Haskell dependencies with ghc-pkg" $ do
        let addHaskellDep yaml =
              yaml
                & key "executables"
                  . key "garner-test"
                  . key "dependencies"
                  . _Array
                  . from vector
                  <>~ ["string-conversions"]
        writeHaskellProject repoDir
        modifyPackageYaml addHaskellDep
        output <- runGarnerWithStdin ["enter", "foo"] "ghc-pkg list | grep string-conversions\nexit\n" repoDir
        dropWhile (== ' ') output `shouldStartWith` "string-conversions"
      it "includes dependencies of simple packages that don't provide an 'env' attribute" $ do
        writeFile
          "garner.ts"
          [i|
            import { mkPackage } from "#{repoDir}/ts/base.ts"

            export const foo = mkPackage({
              attribute: `
                pkgs.stdenv.mkDerivation({
                  name = "blah";
                  src = ./.;
                  buildInputs = [ pkgs.hello ];
                })
              `,
            })
        |]
        output <- runGarnerWithStdin ["enter", "foo"] "hello\nexit\n" repoDir
        output `shouldBe` "Hello, world!\n"

modifyPackageYaml :: (Aeson.Value -> Aeson.Value) -> IO ()
modifyPackageYaml modifier = do
  decoded <- Yaml.decodeFileThrow "package.yaml"
  Yaml.encodeFile "package.yaml" $ modifier decoded

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

runGarnerWithStdin :: [String] -> String -> FilePath -> IO String
runGarnerWithStdin args stdin repoDir = do
  writeFile "stdin" stdin
  capture_ $
    withFile "stdin" ReadMode $ \stdin ->
      withArgs args $
        runWith (Options {stdin, tsRunnerFilename = repoDir <> "/ts/runner.ts"})

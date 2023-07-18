{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GarnerSpec where

import Control.Exception (bracket)
import Control.Lens (from, (<>~))
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
import System.IO (Handle, withFile)
import System.IO.Silently (capture_)
import System.IO.Temp
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory (inTempDirectory)
import Test.Mockery.Environment (withModifiedEnvironment)

spec :: Spec
spec = do
  repoDir <- runIO getCurrentDirectory

  describe "garner" $ around_ inTempDirectory $ do
    describe "run" $ do
      it "runs a simple Haskell program" $ do
        writeHaskellProject repoDir
        output <- runGarner ["run", "foo"] "" repoDir
        output `shouldBe` "haskell test output\n"
      it "writes flake.{lock,nix}, but no other files" $ do
        writeHaskellProject repoDir
        filesBefore <- listDirectory "."
        _ <- runGarner ["run", "foo"] "" repoDir
        filesAfter <- sort <$> listDirectory "."
        filesAfter `shouldBe` sort (filesBefore ++ ["flake.lock", "flake.nix"])
      it "doesn’t format other Nix files" $ do
        let unformattedNix =
              [i|
                    { ...
                      }
                :       {
                  some              =     poorly
                formatted nix;
                      }
              |]
        writeFile "unformatted.nix" unformattedNix
        writeHaskellProject repoDir
        _ <- runGarner ["run", "foo"] "" repoDir
        readFile "./unformatted.nix" `shouldReturn` unformattedNix

    describe "enter" $ do
      it "has the right GHC version" $ do
        writeHaskellProject repoDir
        output <- runGarner ["enter", "foo"] "ghc --numeric-version\nexit\n" repoDir
        output `shouldStartWith` "9.4"
      it "registers Haskell dependencies with ghc-pkg" $ do
        writeHaskellProject repoDir
        modifyPackageYaml $
          key "executables"
            . key "garner-test"
            . key "dependencies"
            . _Array
            . from vector
            <>~ ["string-conversions"]
        output <- runGarner ["enter", "foo"] "ghc-pkg list | grep string-conversions\nexit\n" repoDir
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
        output <- runGarner ["enter", "foo"] "hello\nexit\n" repoDir
        output `shouldBe` "Hello, world!\n"
      it "starts the shell defined in $SHELL" $ do
        writeHaskellProject repoDir
        output <-
          withModifiedEnvironment [("SHELL", "bash")] $
            runGarner ["enter", "foo"] shellTestCommand repoDir
        output `shouldBe` "using bash"
        output <-
          withModifiedEnvironment [("SHELL", "zsh")] $
            runGarner ["enter", "foo"] shellTestCommand repoDir
        output `shouldBe` "using zsh"
      it "defaults to bash" $ do
        writeHaskellProject repoDir
        output <-
          withModifiedEnvironment [("SHELL", "")] $
            runGarner ["enter", "foo"] shellTestCommand repoDir
        output `shouldBe` "using bash"
  -- TODO: Golden tests currently can’t be integrated with the other test cases
  --       because stackbuilders/hspec-golden#40. The case below shows the
  --       effect that @`around_` `inTempDirectory`@ _should_ have.
  describe "garner-golden" $ do
    describe "run" $ do
      it "generates formatted flakes" $ do
        inTempDirectory $ do
          writeHaskellProject repoDir
          _ <- runGarner ["run", "foo"] "" repoDir
          flake <- readFile "./flake.nix"
          pure $ defaultGolden "generates_formatted_flakes" flake

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

runGarner :: [String] -> String -> FilePath -> IO String
runGarner args stdin repoDir = do
  capture_ $
    withTempFile $ \stdin ->
      withArgs args $
        runWith (Options {stdin, tsRunnerFilename = repoDir <> "/ts/runner.ts"})
  where
    withTempFile :: (Handle -> IO a) -> IO a
    withTempFile action =
      bracket
        (writeSystemTempFile "garner-test-stdin" stdin)
        removeFile
        (\file -> withFile file ReadMode action)

shellTestCommand :: String
shellTestCommand =
  [i|
    if [[ -v BASH_VERSION ]]; then
        echo -n "using bash"
    else
        if [[ -v ZSH_VERSION ]]; then
            echo -n "using zsh"
        else
            echo -n "using unknown shell"
        fi
    fi
  |]

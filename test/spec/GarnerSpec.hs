{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GarnerSpec where

import Control.Exception (bracket)
import Control.Lens (from, (<>~))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.List (sort)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Vector.Generic.Lens (vector)
import qualified Data.Yaml as Yaml
import Development.Shake
import Garner
import System.Directory
import System.Environment (withArgs)
import System.IO (Handle, IOMode (..), withFile)
import System.IO.Silently (capture_)
import System.IO.Temp
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory (inTempDirectory)
import Test.Mockery.Environment (withModifiedEnvironment)

spec :: Spec
spec = do
  repoDir <- runIO getCurrentDirectory

  around_ (withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]) $ do
    describe "garner" $ around_ inTempDirectory $ do
      describe "run" $ do
        it "runs a simple Haskell program" $ do
          writeHaskellProject repoDir
          output <- runGarner ["run", "foo"] "" repoDir Nothing
          output `shouldBe` "haskell test output\n"
        it "writes flake.{lock,nix}, but no other files" $ do
          writeHaskellProject repoDir
          filesBefore <- listDirectory "."
          _ <- runGarner ["run", "foo"] "" repoDir Nothing
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
          _ <- runGarner ["run", "foo"] "" repoDir Nothing
          readFile "./unformatted.nix" `shouldReturn` unformattedNix

      describe "enter" $ do
        describe "addDevTools" $ do
          fit "allows dev tools to be added to the dev shell" $ do
            writeHaskellProject repoDir
            modifyGarnerTs $
              \garnerTs ->
                cs
                  [i|
                    import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";
                    #{garnerTs}
                    export const bar = foo.addDevTools([nixpkgs.hello]);
                  |]
            output <- runGarner ["enter", "bar"] "hello -g tool\nexit\n" repoDir
            TIO.putStrLn =<< TIO.readFile "flake.nix"
            output `shouldBe` "tool\n"
          fit "allows multiple dev tools to be added to the dev shell" $ do
            writeHaskellProject repoDir
            modifyGarnerTs $
              \garnerTs ->
                cs
                  [i|
                    import * as nixpkgs from "http://localhost:8777/nixpkgs.ts";
                    #{garnerTs}
                    export const bar = foo.addDevTools([nixpkgs.hello, nixpkgs.cowsay]);
                  |]
            output <- runGarner ["enter", "bar"] "hello -g tool\nexit\n" repoDir
            TIO.putStrLn =<< TIO.readFile "flake.nix"
            output `shouldBe` "tool"
          it "allows dev tools to be added when `env` exists" $ do
            pending -- self.packages.${system}.hello ? env
          it "does not interfere with other packages" $ do
            pending
        it "has the right GHC version" $ do
          writeHaskellProject repoDir
          output <- runGarner ["enter", "foo"] "ghc --numeric-version\nexit\n" repoDir Nothing
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
          output <- runGarner ["enter", "foo"] "ghc-pkg list | grep string-conversions\nexit\n" repoDir Nothing
          dropWhile (== ' ') output `shouldStartWith` "string-conversions"
        it "includes dependencies of simple packages that don't provide an 'env' attribute" $ do
          writeFile
            "garner.ts"
            [i|
              import { mkPackage } from "#{repoDir}/ts/base.ts"

              export const foo = mkPackage({
                expression: `
                  pkgs.stdenv.mkDerivation({
                    name = "blah";
                    src = ./.;
                    buildInputs = [ pkgs.hello ];
                  })
                `,
              })
            |]
          output <- runGarner ["enter", "foo"] "hello\nexit\n" repoDir Nothing
          output `shouldBe` "Hello, world!\n"
        it "starts the shell given by Options.userShell" $ do
          writeHaskellProject repoDir
          StdoutTrim userShell <- cmd ("which bash" :: String)
          output <-
            runGarner ["enter", "foo"] shellTestCommand repoDir $ Just userShell
          output `shouldBe` "using bash"
          StdoutTrim userShell <- cmd ("which zsh" :: String)
          output <-
            runGarner ["enter", "foo"] shellTestCommand repoDir $ Just userShell
          output `shouldBe` "using zsh"

        describe "npm project" $ do
          it "puts node into the $PATH" $ do
            writeFile
              "garner.ts"
              [i|
                import { mkNpmFrontend } from "#{repoDir}/ts/typescript.ts";

                export const frontend = mkNpmFrontend({
                  name: "frontend",
                  src: "./.",
                });
              |]
            stdout <- runGarner ["enter", "frontend"] "node --version" repoDir Nothing
            stdout `shouldStartWith` "v18."
            stdout <- runGarner ["enter", "frontend"] "npm --version" repoDir Nothing
            stdout `shouldStartWith` "9."
    -- TODO: Golden tests currently can’t be integrated with the other test cases
    --       because stackbuilders/hspec-golden#40. The case below shows the
    --       effect that @`around_` `inTempDirectory`@ _should_ have.
    describe "garner-golden" $ do
      describe "run" $ do
        it "generates formatted flakes" $ do
          inTempDirectory $ do
            writeHaskellProject repoDir
            _ <- runGarner ["run", "foo"] "" repoDir Nothing
            flake <- readFile "./flake.nix"
            pure $ defaultGolden "generates_formatted_flakes" flake

modifyPackageYaml :: (Aeson.Value -> Aeson.Value) -> IO ()
modifyPackageYaml modifier = do
  decoded <- Yaml.decodeFileThrow "package.yaml"
  Yaml.encodeFile "package.yaml" $ modifier decoded

modifyGarnerTs :: (Text -> Text) -> IO ()
modifyGarnerTs modifier = do
  garnerTs <- TIO.readFile "garner.ts"
  TIO.writeFile "garner.ts" $ modifier garnerTs

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

runGarner :: [String] -> String -> FilePath -> Maybe FilePath -> IO String
runGarner args stdin repoDir shell = do
  userShell <- maybe (fromStdoutTrim <$> cmd ("which bash" :: String)) pure shell
  capture_ $ withTempFile $ \stdin ->
    withArgs args $
      runWith $
        Options
          { stdin,
            tsRunnerFilename = repoDir <> "/ts/runner.ts",
            userShell
          }
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

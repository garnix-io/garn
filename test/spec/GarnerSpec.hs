{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GarnerSpec where

import Control.Exception (bracket)
import Control.Lens (from, (<>~))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.List (sort)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Vector.Generic.Lens (vector)
import qualified Data.Yaml as Yaml
import Development.Shake
import Garner
import System.Directory
import System.Environment (withArgs)
import System.IO (Handle, IOMode (..), withFile)
import qualified System.IO as Sys
import System.IO.Silently (hCapture, hCapture_)
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
          stdout output `shouldBe` "haskell test output\n"
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
          it "allows dev tools to be added to the dev shell" $ do
            writeHaskellProject repoDir
            writeFile "garner.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/base.ts"
                  import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

                  export const foo = mkHaskell({
                    name: "mkHaskell-test",
                    executable: "garner-test",
                    compiler: "ghc94",
                    src: "./."
                  })
                  const hello = mkPackage({
                    expression: `pkgs.hello`,
                  });

                  export const bar = foo.addDevTools([hello]);
                |]
            output <- runGarner ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"
          it "allows multiple dev tools to be added to the dev shell" $ do
            writeHaskellProject repoDir
            writeFile "garner.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/base.ts"
                  import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

                  export const foo = mkHaskell({
                    name: "mkHaskell-test",
                    executable: "garner-test",
                    compiler: "ghc94",
                    src: "./."
                  })

                  const hello = mkPackage({
                    expression: `pkgs.hello`,
                  });

                  const cowsay = mkPackage({
                    expression: `pkgs.cowsay`,
                  });

                  export const bar = foo.addDevTools([hello, cowsay]);
                |]
            output <- runGarner ["enter", "bar"] "hello -g tool\nexit\n" repoDir Nothing
            stdout output `shouldBe` "tool\n"
            output <- runGarner ["enter", "bar"] "which cowsay\nexit\n" repoDir Nothing
            stdout output `shouldStartWith` "/nix/store"
          it "does not destructively update the given package" $ do
            writeHaskellProject repoDir
            writeFile "garner.ts" $
              unindent
                [i|
                  import { mkPackage } from "#{repoDir}/ts/base.ts"
                  import { mkHaskell } from "#{repoDir}/ts/haskell.ts"

                  export const foo = mkHaskell({
                    name: "mkHaskell-test",
                    executable: "garner-test",
                    compiler: "ghc94",
                    src: "./."
                  })

                  const hello = mkPackage({
                    expression: `pkgs.hello`,
                  });

                  export const bar = foo.addDevTools([hello]);
                |]
            output <- runGarner ["enter", "foo"] "hello -g tool\nexit\n" repoDir Nothing
            stderr output `shouldContain` "hello: command not found"
        it "has the right GHC version" $ do
          writeHaskellProject repoDir
          output <- runGarner ["enter", "foo"] "ghc --numeric-version\nexit\n" repoDir Nothing
          stdout output `shouldStartWith` "9.4"
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
          dropWhile (== ' ') (stdout output) `shouldStartWith` "string-conversions"
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
          stdout output `shouldBe` "Hello, world!\n"
        it "starts the shell defined in $SHELL" $ do
          writeHaskellProject repoDir
          StdoutTrim userShell <- cmd ("which bash" :: String)
          output <-
            runGarner ["enter", "foo"] shellTestCommand repoDir $ Just userShell
          stdout output `shouldBe` "using bash"
          StdoutTrim userShell <- cmd ("which zsh" :: String)
          output <-
            runGarner ["enter", "foo"] shellTestCommand repoDir $ Just userShell
          stdout output `shouldBe` "using zsh"

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
            output <- runGarner ["enter", "frontend"] "node --version" repoDir Nothing
            stdout output `shouldStartWith` "v18."
            output <- runGarner ["enter", "frontend"] "npm --version" repoDir Nothing
            stdout output `shouldStartWith` "9."

      describe "start" $ do
        describe "no start command configured" $ do
          it "runs npm start" $ do
            writeNpmFrontendProject repoDir
            output <- runGarner ["start", "frontend"] "" repoDir Nothing
            stderr output `shouldBe` "[garner] Running \"npm run start\"\n"
            stdout output
              `shouldBe` unindent
                [i|

                  > frontend@0.0.1 start
                  > echo running

                  running
                |]
          it "displays an error if the script exists but exits with non-zero status code" $ do
            writeNpmFrontendProject repoDir
            writeFile
              "package.json"
              [i|
                {
                  "name": "frontend",
                  "version": "0.0.1",
                  "scripts": {
                    "start": "exit 42"
                  }
                }
              |]
            output <- runGarner ["start", "frontend"] "" repoDir Nothing
            stderr output
              `shouldBe` unindent
                [i|
                  [garner] Running "npm run start"
                  [garner] "npm run start" exited with status code 42
                |]

        describe "with a specific start command configured" $ do
          it "runs the configured command" $ do
            writeNpmFrontendProject repoDir
            writeFile
              "garner.ts"
              [i|
                import { mkNpmFrontend } from "#{repoDir}/ts/typescript.ts";

                export const frontend = mkNpmFrontend({
                  name: "mkNpmFrontend-custom-command-test",
                  src: "./.",
                }).setStartCommand(["echo", "foobar"]);
              |]
            output <- runGarner ["start", "frontend"] "" repoDir Nothing
            stderr output `shouldBe` "[garner] Running \"echo foobar\"\n"
            stdout output `shouldBe` "foobar\n"
          it "correctly handles argv arrays" $ do
            writeFile
              "garner.ts"
              [i|
                import { mkNpmFrontend } from "#{repoDir}/ts/typescript.ts";

                export const frontend = mkNpmFrontend({
                  name: "mkNpmFrontend-custom-command-test",
                  src: "./.",
                }).setStartCommand(["printf", "%s..%s..%s", "foo bar", "baz"]);
              |]
            output <- runGarner ["start", "frontend"] "" repoDir Nothing
            stdout output `shouldBe` "foo bar..baz.."
          it "displays an error if the command exits with non-zero status code" $ do
            writeNpmFrontendProject repoDir
            writeFile
              "garner.ts"
              [i|
                import { mkNpmFrontend } from "#{repoDir}/ts/typescript.ts";

                export const frontend = mkNpmFrontend({
                  name: "mkNpmFrontend-custom-command-test",
                  src: "./.",
                }).setStartCommand(["sh", "-c", ">&2 echo error! && exit 42"]);
              |]
            output <- runGarner ["start", "frontend"] "" repoDir Nothing
            stderr output
              `shouldBe` unindent
                [i|
                  [garner] Running "sh -c >&2 echo error! && exit 42"
                  error!
                  [garner] "sh -c >&2 echo error! && exit 42" exited with status code 42
                |]

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

writeNpmFrontendProject :: FilePath -> IO ()
writeNpmFrontendProject repoDir = do
  writeFile
    "garner.ts"
    [i|
      import { mkNpmFrontend } from "#{repoDir}/ts/typescript.ts"

      export const frontend = mkNpmFrontend({
        name: "mkNpmFrontend-test",
        src: "./."
      })
    |]
  writeFile
    "package.json"
    [i|
      {
        "name": "frontend",
        "version": "0.0.1",
        "scripts": {
          "start": "echo running"
        }
      }
    |]

runGarner :: [String] -> String -> FilePath -> Maybe FilePath -> IO ProcResult
runGarner args stdin repoDir shell = do
  userShell <- maybe (fromStdoutTrim <$> cmd ("which bash" :: String)) pure shell
  (stderr, stdout) <- hCapture [Sys.stderr] $
    hCapture_ [Sys.stdout] $
      withTempFile $ \stdin ->
        withArgs args $
          runWith (Options {stdin, userShell, tsRunnerFilename = repoDir <> "/ts/runner.ts"})
  return $ ProcResult {..}
  where
    withTempFile :: (Handle -> IO a) -> IO a
    withTempFile action =
      bracket
        (writeSystemTempFile "garner-test-stdin" stdin)
        removeFile
        (\file -> withFile file ReadMode action)

data ProcResult = ProcResult
  { stdout :: String,
    stderr :: String
  }

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

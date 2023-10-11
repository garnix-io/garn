{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GarnSpec where

import Control.Monad (forM_, unless)
import Data.List (sort)
import Data.String.Conversions (cs)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Development.Shake (StdoutTrim (..), cmd)
import System.Directory
import System.Exit (ExitCode (..))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import Test.Mockery.Directory (inTempDirectory)
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils
import Text.Regex.PCRE.Heavy (compileM, (=~))

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
                  gen.*
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
                  gen
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

      describe "build" $ do
        it "builds packages and creates a result link" $ do
          writeHaskellProject repoDir
          _ <- runGarn ["build", "foo"] "" repoDir Nothing
          doesDirectoryExist "result" `shouldReturn` True
          StdoutTrim output <- cmd ("result/bin/garn-test" :: String)
          output `shouldBe` ("haskell test output" :: String)

        it "complains about packages that cannot be built" $ do
          writeHaskellProject repoDir
          writeFile
            "Main.hs"
            [i|
              main :: IO ()
              main = "foo"
            |]
          output <- runGarn ["build", "foo"] "" repoDir Nothing
          stderr output `shouldContain` "Couldn't match type"
          exitCode output `shouldBe` ExitFailure 1

      describe "run" $ do
        it "runs a simple Haskell program" $ do
          writeHaskellProject repoDir
          output <- runGarn ["run", "foo"] "" repoDir Nothing
          stdout output `shouldBe` "haskell test output\n"
        it "writes flake.{lock,nix}, but no other files" $ do
          writeHaskellProject repoDir
          filesBefore <- listDirectory "."
          _ <- runGarn ["run", "foo"] "" repoDir Nothing
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
          _ <- runGarn ["run", "foo"] "" repoDir Nothing
          readFile "./unformatted.nix" `shouldReturn` unformattedNix

      describe "check" $ do
        it "runs manually added checks" $ do
          writeHaskellProject repoDir
          writeFile
            "Main.hs"
            [i|
              main :: IO ()
              main = return ()

              f :: [Int] -> [Int]
              f list = map (+ 1) list
            |]
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              const haskellBase = garn.haskell.mkHaskell({
                description: "mkHaskell-test",
                executable: "garn-test",
                compiler: "ghc94",
                src: "."
              }).withDevTools([garn.mkPackage(`pkgs.hlint`)]);

              export const haskell = {
                ...haskellBase,
                hlint: haskellBase.check`hlint *.hs`,
              };
            |]
          output <- runGarn ["check", "haskell"] "" repoDir Nothing
          stderr output `shouldContain` "Warning: Eta reduce"
        it "runs checks on source directories that ignore the flake.nix file" $ do
          writeHaskellProject repoDir
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              const haskellBase = garn.haskell.mkHaskell({
                description: "mkHaskell-test",
                executable: "garn-test",
                compiler: "ghc94",
                src: "."
              });

              export const haskell = {
                ...haskellBase,
                hlint: haskellBase.check`
                  ls
                  false
                `,
              };
            |]
          output <- runGarn ["check", "haskell"] "" repoDir Nothing
          stderr output `shouldNotContain` "flake.nix"
        describe "exit-codes" $ do
          let testCases =
                [ ("passing", "true", ExitSuccess),
                  ("failing", "false", ExitFailure 1),
                  ("unknownCommand", "does-not-exist", ExitFailure 1),
                  ("failureBeforeOtherCommands", "false; true", ExitFailure 1),
                  ("negated", "! true", ExitFailure 1),
                  ("pipefail", "false | true", ExitFailure 1)
                ]
          forM_ testCases $ \(checkName, check :: String, expectedExitCode) -> do
            it ("reports exit-codes correctly for check '" <> checkName <> "'") $ do
              writeHaskellProject repoDir
              writeFile
                "garn.ts"
                [i|
                  import * as garn from "#{repoDir}/ts/mod.ts"

                  const haskellBase = garn.haskell.mkHaskell({
                    description: "mkHaskell-test",
                    executable: "garn-test",
                    compiler: "ghc94",
                    src: "."
                  });

                  export const haskell = {
                    ...haskellBase,
                    check: haskellBase.check`#{check}`,
                  };
                |]
              result <- runGarn ["check", "haskell"] "" repoDir Nothing
              putStrLn $ stdout result
              putStrLn $ stderr result
              stderr result `shouldNotContain` "Invalid argument"
              exitCode result `shouldBe` expectedExitCode

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

shouldMatch :: (HasCallStack) => String -> String -> Expectation
shouldMatch actual expected = case compileM (cs expected) [] of
  Left err -> expectationFailure $ "invalid regex: " <> show err
  Right regex ->
    unless (actual =~ regex) $
      expectationFailure $
        "expected " <> actual <> " to match regex " <> show expected

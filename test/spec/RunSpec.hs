{-# LANGUAGE QuasiQuotes #-}

module RunSpec where

import Data.List (sort)
import Data.String.Interpolate (i)
import System.Directory
import System.Exit (ExitCode (..))
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils

spec :: Spec
spec =
  describe "run" $ do
    repoDir <- runIO getCurrentDirectory
    around_
      ( withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]
          . inTempDirectory
      )
      $ do
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
        it "runs arbitrary executables" $ do
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              export const main = garn.mkProject('Project with an executable', {
                print: garn.shell`echo foobarbaz`,
              }, {
                defaults: {
                  executable: 'print',
                },
              });
            |]
          output <- runGarn ["run", "main"] "" repoDir Nothing
          stdout output `shouldBe` "foobarbaz\n"
          exitCode output `shouldBe` ExitSuccess
        it "runs executables within an environment" $ do
          writeFile
            "garn.ts"
            [i|
              import * as garn from "#{repoDir}/ts/mod.ts"

              const myEnv = garn.mkEnvironment(
                'pkgs.mkShell { nativeBuildInputs = [pkgs.hello]; }',
                '.'
              );
              export const main = garn.mkProject('Project with an executable', {
                sayHello: myEnv.shell`hello`,
              }, {
                defaults: {
                  executable: 'sayHello',
                },
              });
            |]
          output <- runGarn ["run", "main"] "" repoDir Nothing
          stdout output `shouldBe` "Hello, world!\n"
          exitCode output `shouldBe` ExitSuccess

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
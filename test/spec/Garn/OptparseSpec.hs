module Garn.OptparseSpec where

import Data.Map as Map
import Garn.GarnConfig
import Garn.Optparse
import System.Environment (withArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (stderr)
import System.IO.Silently (hSilence)
import Test.Hspec

spec :: Spec
spec = around_ (hSilence [stderr]) $ do
  describe "Garn.Optparse" $ do
    describe "generate subcommand" $ do
      it "parses generate commands" $ do
        command <- testWithGarnTs ["generate"] mempty
        command `shouldBe` Gen

    describe "check subcommand" $ do
      it "parses qualified check commands" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = []
                  }
        command <- testWithGarnTs ["check", "project"] ("project" ~> targetConfig)
        command `shouldBe` Check (Qualified (CommandOptions "project" targetConfig))

      it "parses unqualified check commands" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = []
                  }
        command <- testWithGarnTs ["check"] ("project" ~> targetConfig)
        command `shouldBe` Check Unqualified

      it "errors on non-existing targets" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = []
                  }
        testWithGarnTs ["check", "does-not-exist"] ("project" ~> targetConfig)
          `shouldThrow` (== ExitFailure 1)

    describe "run subcommand" $ do
      it "parses run commands" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = []
                  }
        command <- testWithGarnTs ["run", "project"] ("project" ~> targetConfig)
        command `shouldBe` Run (CommandOptions "project" targetConfig) []

      it "parses run commands with additional arguments" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = []
                  }
        command <- testWithGarnTs ["run", "project", "more", "args"] ("project" ~> targetConfig)
        command `shouldBe` Run (CommandOptions "project" targetConfig) ["more", "args"]

      it "parses run commands with additional flags (starting with `-`)" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = []
                  }
        command <- testWithGarnTs ["run", "project", "--flag"] ("project" ~> targetConfig)
        command `shouldBe` Run (CommandOptions "project" targetConfig) ["--flag"]
        command <- testWithGarnTs ["run", "project", "-f"] ("project" ~> targetConfig)
        command `shouldBe` Run (CommandOptions "project" targetConfig) ["-f"]

      it "parses --help as garn option, unless separated by `--`" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = []
                  }
        testWithGarnTs ["run", "project", "--help"] ("project" ~> targetConfig)
          `shouldThrow` (== ExitSuccess)
        command <- testWithGarnTs ["run", "project", "--", "--help"] ("project" ~> targetConfig)
        command `shouldBe` Run (CommandOptions "project" targetConfig) ["--help"]

testWithGarnTs :: [String] -> Targets -> IO WithGarnTsCommand
testWithGarnTs args targets = do
  options <- withArgs args $ getOpts $ WithGarnTs $ GarnConfig targets "test flake file"
  pure $ case options of
    WithGarnTsOpts _ command -> command
    _ -> error "Expected WithGarnTsOpts"

(~>) :: key -> value -> Map key value
(~>) = Map.singleton

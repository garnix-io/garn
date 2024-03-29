module Garn.OptparseSpec where

import Data.Map as Map
import Garn.GarnConfig
import Garn.Optparse
import System.Environment (withArgs)
import System.Exit (ExitCode (ExitFailure))
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
                    checks = [],
                    runnable = False
                  }
        command <- testWithGarnTs ["check", "project"] ("project" ~> targetConfig)
        command `shouldBe` Check (Qualified (CommandOptions (fromUserFacing "project") targetConfig))

      it "parses unqualified check commands" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = [],
                    runnable = False
                  }
        command <- testWithGarnTs ["check"] ("project" ~> targetConfig)
        command `shouldBe` Check Unqualified

      it "errors on non-existing targets" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = [],
                    runnable = False
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
                    checks = [],
                    runnable = True
                  }
        command <- testWithGarnTs ["run", "project"] ("project" ~> targetConfig)
        command `shouldBe` Run (CommandOptions (fromUserFacing "project") targetConfig) []

      it "parses run commands with additional arguments" $ do
        let targetConfig =
              TargetConfigProject $
                ProjectTarget
                  { description = "test project",
                    packages = [],
                    checks = [],
                    runnable = True
                  }
        command <- testWithGarnTs ["run", "project", "more", "args"] ("project" ~> targetConfig)
        command `shouldBe` Run (CommandOptions (fromUserFacing "project") targetConfig) ["more", "args"]

testWithGarnTs :: [String] -> Targets -> IO WithGarnTsCommand
testWithGarnTs args targets = do
  options <- withArgs args $ getOpts $ WithGarnTs $ GarnConfig targets "test flake file"
  pure $ case options of
    WithGarnTsOpts _ command -> command
    _ -> error "Expected WithGarnTsOpts"

(~>) :: String -> value -> Map TargetName value
k ~> v = Map.singleton (TargetName k) v

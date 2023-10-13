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
    describe "check subcommand" $ do
      it "parses gen commands" $ do
        command <- testWithGarnTs ["gen"] mempty
        command `shouldBe` Gen

      it "parses qualified check commands" $ do
        let targetConfig =
              TargetConfig
                { description = "test project",
                  packages = [],
                  checks = []
                }
        command <- testWithGarnTs ["check", "project"] ("project" ~> targetConfig)
        command `shouldBe` Check (Qualified (CommandOptions "project" targetConfig))

      it "parses unqualified check commands" $ do
        let targetConfig =
              TargetConfig
                { description = "test project",
                  packages = [],
                  checks = []
                }
        command <- testWithGarnTs ["check"] ("project" ~> targetConfig)
        command `shouldBe` Check Unqualified

      it "errors on non-existing targets" $ do
        let targetConfig =
              TargetConfig
                { description = "test project",
                  packages = [],
                  checks = []
                }
        testWithGarnTs ["check", "does-not-exist"] ("project" ~> targetConfig)
          `shouldThrow` (== ExitFailure 1)

testWithGarnTs :: [String] -> Targets -> IO WithGarnTsCommand
testWithGarnTs args targets = do
  options <- withArgs args $ getOpts $ WithGarnTs $ GarnConfig targets "test flake file"
  pure $ case options of
    WithGarnTsOpts _ command -> command
    _ -> error "Expected WithGarnTsOpts"

(~>) :: key -> value -> Map key value
(~>) = Map.singleton

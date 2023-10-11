{-# LANGUAGE QuasiQuotes #-}

module BuildSpec where

import Data.String.Interpolate (i)
import Development.Shake (StdoutTrim (..), cmd)
import System.Directory
import System.Exit (ExitCode (..))
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils

spec :: Spec
spec = do
  describe "build" $ do
    repoDir <- runIO getCurrentDirectory
    around_
      ( withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]
          . inTempDirectory
      )
      $ do
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

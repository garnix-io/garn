{-# LANGUAGE QuasiQuotes #-}

module EditSpec where

import System.Directory
import Test.Hspec
import Test.Mockery.Directory
import Test.Mockery.Environment (withModifiedEnvironment)
import TestUtils

spec :: Spec
spec =
  describe "edit" $ around onTestFailureLogger $ do
    repoDir <- runIO getCurrentDirectory
    around_
      ( withModifiedEnvironment [("NIX_CONFIG", "experimental-features =")]
          . inTempDirectory
      )
      $ do
        it "starts VSCodium with the deno extension" $ \onTestFailureLog -> do
          output <- runGarn ["edit", "--", "--list-extensions"] "" repoDir Nothing
          onTestFailureLog output
          stdout output `shouldBe` "denoland.vscode-deno\n"
        it "works even when garn.ts has a syntax error" $ \onTestFailureLog -> do
          writeFile "garn.ts" "kkvjh{ shyntax err"
          output <- runGarn ["edit", "--", "--list-extensions"] "" repoDir Nothing
          onTestFailureLog output
          stdout output `shouldBe` "denoland.vscode-deno\n"

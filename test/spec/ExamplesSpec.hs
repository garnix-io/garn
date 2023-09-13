{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module ExamplesSpec where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Development.Shake
import Network.HTTP.Client (HttpException, responseBody)
import Network.Wreq (Response, get)
import System.Process
import Test.Hspec

withCmd :: IO ProcessHandle -> IO () -> IO ()
withCmd cmd action = bracket cmd interruptProcessGroupOf $ const action

retryGet :: String -> IO (Response Text)
retryGet url = fmap cs <$> go 100
  where
    go (0 :: Int) = error "retryGet: failed after 100 tries"
    go n = do
      catch (get url) $ \(_ :: HttpException) -> do
        threadDelay 100000
        go (n - 1)

spec :: Spec
spec = do
  describe "frontend-create-react-app" $ do
    describe "garner start" $ do
      it "serves a page" $ do
        withCmd (cmd (Cwd "test-examples/frontend-create-react-app") "cabal run garner:garner start main") $ do
          body <- responseBody <$> retryGet "http://localhost:3000"
          body `shouldBe` "<!DOCTYPE html>\n<html lang=\"en\"><head><script defer src=\"/static/js/bundle.js\"></script></head>\n  <body>\n    Hello from create-react-app\n  </body>\n</html>\n"

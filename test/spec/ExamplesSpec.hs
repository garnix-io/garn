{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module ExamplesSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (SomeException, bracket, catch, throwIO)
import Control.Monad (forM_)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Development.Shake
import Network.HTTP.Client (HttpException, responseBody)
import Network.Wreq (Response, get)
import System.Directory (listDirectory)
import System.FilePath
import System.Process
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

raceCatch :: IO a -> IO b -> IO (Either (Either SomeException a) (Either SomeException b))
raceCatch left right =
  withAsync left $ \a ->
    withAsync right $ \b ->
      waitEitherCatch a b

withCmd :: IO ProcessHandle -> IO a -> IO a
withCmd cmd action = do
  result <-
    raceCatch
      (bracket cmd interruptProcessGroupOf waitForProcess)
      action
  case result of
    Left result -> do
      expectationFailure $ "cmd exited before action: " <> show result
      pure undefined
    Right (Right a) -> pure a
    Right (Left exception) -> throwIO exception

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
  dirs <- runIO $ listDirectory "test-examples"
  forM_ dirs $ \dir -> do
    describe dir $ do
      describe "garner start" $ do
        it "serves a page" $ do
          withCmd (cmd (Cwd ("test-examples" </> dir)) "cabal run garner:garner start main") $ do
            body <- responseBody <$> retryGet "http://localhost:3000"
            pure $ defaultGolden dir (cs body)

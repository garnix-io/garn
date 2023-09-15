module ExampleSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (waitEitherCatch, withAsync)
import Control.Exception (SomeException, bracket, catch, throwIO)
import Control.Lens ((^.))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Development.Shake
import Network.HTTP.Client (HttpException)
import Network.Wreq (Response, get, responseBody)
import System.Process (ProcessHandle, interruptProcessGroupOf, waitForProcess)
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
retryGet url = fmap cs <$> go 1000
  where
    go (0 :: Int) = error "retryGet: failed after 100 tries"
    go n = do
      catch (get url) $ \(_ :: HttpException) -> do
        threadDelay 100000
        go (n - 1)

startFileServer :: IO ()
startFileServer = do
  _ :: ProcessHandle <- cmd "just fileserver"
  pure ()

spec :: Spec
spec = do
  describe "frontend-yarn-webpack" $ do
    describe "run" $ do
      it "starts the frontend" $ do
        withCmd
          ( cmd
              (Cwd "examples/frontend-yarn-webpack")
              "cabal run garner:garner --"
              "run frontend"
          )
          $ do
            body <- (^. responseBody) <$> retryGet "http://localhost:3000"
            pure $ defaultGolden "frontend-yarn-webpack" (cs body)

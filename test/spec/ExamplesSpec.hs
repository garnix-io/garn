module ExamplesSpec where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch)
import Control.Monad (void)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Development.Shake
import Network.HTTP.Client (HttpException)
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
  it "???" $ do
    withCmd (cmd (Cwd "test-examples/frontend-create-react-app") ("cabal run garner:garner start main" :: String)) $ do
      void $ retryGet $ "http://localhost:3000"

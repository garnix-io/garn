module ExamplesSpec where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Development.Shake
import Control.Exception (bracket)
import System.Process
import Test.Hspec
import Network.Wreq (get)

withCmd :: IO ProcessHandle -> IO () -> IO ()
withCmd cmd action = bracket cmd terminateProcess $ const action

retryUntilPasses :: IO () -> IO ()
retryUntilPasses action = action

spec :: Spec
spec = do
  it "???" $ do
    withCmd (cmd (Cwd "test-examples/frontend-create-react-app") ("cabal run garner:garner start main" :: String)) $ do
      retryUntilPasses $ do
        get $ "http://localhost:" <> show 3000
        pure ()

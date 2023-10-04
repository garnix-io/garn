import Control.Concurrent
import Control.Monad
import Data.Function
import Development.Shake
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import System.Directory
import System.Exit
import System.FSNotify
import System.FilePath
import System.IO
import WithCli

main :: IO ()
main = withCli $ do
  isRunning >>= \yes -> when yes $ do
    error "Fileserver already running"
  let settings =
        defaultSettings
          & setPort port
          & setBeforeMainLoop
            ( do
                putStrLn $ "listening on port " <> show port
                startDenoCacheInvalidator
            )
  runSettings settings $ staticApp $ defaultFileServerSettings "ts"

isRunning :: IO Bool
isRunning = do
  Exit code <-
    cmd
      "curl"
      ("http://localhost:" <> show port)
      (EchoStdout False)
      (EchoStderr False)
  pure (code == ExitSuccess)

port :: Int
port = 8777

startDenoCacheInvalidator :: IO ()
startDenoCacheInvalidator = void $ forkIO $ do
  withManager $ \manager -> do
    -- invalidate all files on startup
    tsFiles <- listDirectory "ts"
    forM_ tsFiles reload
    hPutStrLn stderr "initial reloading done"
    -- invalidate individual files on changes
    tsDirectory <- makeAbsolute "ts"
    _ <- watchDir manager "ts" (const True) $ \event -> do
      when (takeExtension (eventPath event) == ".ts") $ do
        reload $ makeRelative tsDirectory $ eventPath event
    forever $ threadDelay 1000000

reload :: FilePath -> IO ()
reload file = do
  let url = "http://localhost:8777/" <> file
  Exit _ <- cmd "deno cache --reload" url
  pure ()

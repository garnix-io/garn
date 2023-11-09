import Control.Concurrent
import Control.Monad
import Cradle
import Data.Function
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
  (code, StdoutUntrimmed _, Stderr _) <-
    Cradle.run "curl" ("http://localhost:" <> show port)
  pure (code == ExitSuccess)

port :: Int
port = 8777

startDenoCacheInvalidator :: IO ()
startDenoCacheInvalidator = void $ forkIO $ do
  withManager $ \manager -> do
    -- invalidate all files on startup
    tsFiles <- listDirectoryRecursive "ts"
    forM_ tsFiles reload
    hPutStrLn stderr "initial reloading done"
    -- invalidate individual files on changes
    tsDirectory <- makeAbsolute "ts"
    _ <- watchTree manager "ts" (const True) $ \event -> do
      when (takeExtension (eventPath event) == ".ts") $ do
        reload $ makeRelative tsDirectory $ eventPath event
    forever $ threadDelay 1000000

reload :: FilePath -> IO ()
reload file = do
  let url = "http://localhost:8777/" <> file
  _ :: ExitCode <- Cradle.run "deno" "cache" ("--reload=" <> url) url
  pure ()

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive anchor =
  go "."
  where
    go path = do
      isFile <- doesFileExist (anchor </> path)
      isDir <- doesDirectoryExist (anchor </> path)
      case (isFile, isDir) of
        (True, False) -> pure [path]
        (False, True) -> do
          entries <- listDirectory (anchor </> path)
          mconcat <$> mapM go (map (path </>) entries)
        _ -> error ("not a file or directory: " <> path)

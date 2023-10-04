module Garner.Init (initGarnerTs) where

import Development.Shake (cmd_)
import System.Environment (lookupEnv)

initGarnerTs :: FilePath -> IO ()
initGarnerTs initFilePath = do
  imap <- lookupEnv "DENO_IMPORT_MAP"
  let importMap = case imap of
        Nothing -> ""
        Just imap -> "--import-map " <> imap
  cmd_ $ "deno run " <> importMap <> " --quiet --check --allow-write --allow-read --allow-run " <> initFilePath

module Garner.Init (initGarnerTs) where

import Development.Shake (cmd_)

initGarnerTs :: FilePath -> IO ()
initGarnerTs initFilePath = do
  cmd_ "deno run --quiet --check --allow-write --allow-read --allow-run" initFilePath

module Garn.Init (initGarnTs) where

import Development.Shake (cmd_)

initGarnTs :: FilePath -> IO ()
initGarnTs initFilePath = do
  cmd_ "deno run --quiet --check --allow-write --allow-read --allow-run" initFilePath

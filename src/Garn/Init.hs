module Garn.Init (initGarnTs) where

import Cradle (run_)

initGarnTs :: FilePath -> IO ()
initGarnTs initFilePath = do
  run_ (words "deno run --quiet --check --allow-write --allow-read --allow-run") initFilePath

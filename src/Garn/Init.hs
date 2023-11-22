module Garn.Init (initGarnTs) where

import Cradle (StderrHandle (StderrHandle), run_)
import Garn.Env (Env (..))

initGarnTs :: Env -> FilePath -> IO ()
initGarnTs env initFilePath = do
  run_
    (StderrHandle (stderr env))
    (words "deno run --quiet --check --allow-write --allow-read --allow-run")
    initFilePath

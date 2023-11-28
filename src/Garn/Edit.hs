module Garn.Edit (editGarnTs) where

import Cradle (StdoutTrimmed (..), run, run_)
import Data.String.Conversions (cs)
import Paths_garn (getDataFileName)

editGarnTs :: [String] -> IO ()
editGarnTs args = do
  editorNix <- getDataFileName "nix/editor.nix"
  StdoutTrimmed path <- run "nix-build" [cs editorNix, "--no-out-link"]
  run_ (cs path <> "/bin/vscodium") ("garn.ts" : args)

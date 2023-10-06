module Garn.Utils where

import Debug.Trace (trace)

dbg :: (Show a) => a -> a
dbg a = trace (show a) a

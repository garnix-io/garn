module Garn.Utils where

import Data.String.Conversions (cs)
import Debug.Trace (trace)
import Text.Pretty.Simple (pShow)

dbg :: (Show a) => a -> a
dbg a = trace (cs $ pShow a) a

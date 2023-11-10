{-# LANGUAGE TemplateHaskell #-}

module Garn.Utils where

import Data.String.Conversions (cs)
import Debug.Trace (trace)
import Garn.ImportVersion (garnVersionSplice)
import Text.Pretty.Simple (pShow)

dbg :: (Show a) => a -> a
dbg a = trace (cs $ pShow a) a

garnCliVersion :: String
garnCliVersion = $(garnVersionSplice)

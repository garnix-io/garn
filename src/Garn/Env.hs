module Garn.Env where

import System.IO (Handle)

data Env = Env
  { workingDir :: FilePath,
    args :: [String],
    stdin :: Handle,
    stdout :: Handle,
    stderr :: Handle,
    initFileName :: FilePath,
    userShell :: FilePath
  }

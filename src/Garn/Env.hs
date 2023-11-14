module Garn.Env where

import System.IO (Handle)

data Env = Env
  { workingDir :: FilePath,
    stdin :: Handle,
    stderr :: Handle,
    initFileName :: FilePath,
    userShell :: FilePath
  }

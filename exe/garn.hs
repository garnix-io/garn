module Main where

import Garn (run)
import Garn.Env (Env (..))
import Paths_garn (getDataFileName)
import System.Environment (getArgs)
import qualified System.IO
import qualified System.Posix.User as POSIX

main :: IO ()
main = do
  env <- productionEnv
  run env

productionEnv :: IO Env
productionEnv = do
  initFileName <- getDataFileName "ts/internal/init.ts"
  userShell <- findUserShell
  args <- getArgs
  pure $
    Env
      { workingDir = ".",
        args,
        stdin = System.IO.stdin,
        stdout = System.IO.stdout,
        stderr = System.IO.stderr,
        initFileName,
        userShell
      }

findUserShell :: IO FilePath
findUserShell = do
  userId <- POSIX.getRealUserID
  POSIX.userShell <$> POSIX.getUserEntryForID userId

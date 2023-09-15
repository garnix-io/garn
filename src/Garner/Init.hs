module Garner.Init where

import Development.Shake (cmd)

init :: IO ()
init = cmd "deno run --quiet init.ts"

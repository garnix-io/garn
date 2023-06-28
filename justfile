list:
  just --list

ci: hpack fmt test codegen check-examples

fmt: fmt-nix fmt-haskell fmt-typescript

fmt-nix:
  nixpkgs-fmt *.nix

fmt-haskell:
  #!/usr/bin/env bash

  set -eux

  ormolu \
    --mode inplace \
    $(find . -name '*.hs' | grep -v '^./dist-newstyle/')
  fhi $(find . -name '*.hs' | grep -v '^./dist-newstyle/')

fmt-typescript:
  prettier --write $(fd .ts ts | grep -v nixpkgs.ts)

hpack:
  hpack

test: hpack
  cabal test --test-show-details=streaming

watch *args="": hpack
  #!/usr/bin/env bash

  ghcid --command "cabal repl test:spec" --test ':main {{ args }}' --warnings


fileserver:
  #!/usr/bin/env runhaskell

  import Control.Monad
  import Development.Shake
  import Network.Wai.Application.Static
  import Network.Wai.Handler.Warp
  import System.Exit

  main :: IO ()
  main = do
    isRunning >>= \yes -> when yes $ do
      error "Fileserver already running"
    putStrLn "Starting server"
    run port $ staticApp $ defaultFileServerSettings "ts"

  isRunning :: IO Bool
  isRunning = do
    Exit code <-
      cmd
        "curl" ("http://localhost:" <> show port)
        (EchoStdout False)
        (EchoStderr False)
    pure (code == ExitSuccess)

  port :: Int
  port = 8777


run-example *args="run haskellExecutable": hpack
  #!/usr/bin/env bash

  set -eux

  cd examples
  cabal run garner:garner -- {{ args }}

check-examples:
  just run-example run haskellExecutable
  just run-example run hello

codegen: hpack
  cabal run codegen
  deno check ts/*.ts
  echo done

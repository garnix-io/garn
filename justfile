list:
  just --list

ci: hpack fmt test codegen check-examples

fmt: fmt-nix fmt-haskell fmt-typescript

check: fmt-nix-check fmt-haskell-check hpack-check fmt-typescript-check

fmt-nix:
  nixpkgs-fmt *.nix

fmt-nix-check:
  nixpkgs-fmt --check .

fmt-haskell:
  just ormolu inplace

fmt-haskell-check:
  just ormolu check

ormolu mode: hpack
  #!/usr/bin/env bash

  set -eux

  ormolu \
    --mode {{ mode }} \
    $(find . -name '*.hs' | grep -v '^./dist-newstyle/')
  if [[ "{{ mode }}" == inplace ]]; then
    fhi $(find . -name '*.hs' | grep -v '^./dist-newstyle/')
  fi

fmt-typescript:
  prettier --write $(fd .ts ts | grep -v nixpkgs.ts)

fmt-typescript-check:
  prettier --check $(fd .ts ts | grep -v nixpkgs.ts)

hpack:
  hpack

hpack-check:
  #!/usr/bin/env runhaskell

  import Control.Monad
  import System.Process

  main = do
    oldCabal <- readFile "garner.cabal"
    newCabal <- readProcess "hpack" (words "-") ""
    when (oldCabal /= newCabal) $
      error "package.yaml has changed, please run hpack"

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

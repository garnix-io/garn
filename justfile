# Show the available commands
list:
  just --list

# Check what we can before pushing changes
pre-push: fmt github-ci

fast-pre-push: fmt test check-examples typescript-check test-ts

# Run checks that we can’t yet run via the flake
github-ci: test codegen check-isolated-garner check-examples test-ts

fmt: fmt-nix fmt-haskell fmt-typescript

check: fmt-nix-check fmt-haskell-check hpack-check fmt-typescript-check

fmt-nix:
  nixpkgs-fmt .

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

test-ts:
  deno test --allow-write --allow-read --allow-run ts/*.ts

test *args="": hpack
  cabal run --test-show-details=streaming garner:spec -- {{ args }}

# Run the tests continuously as the code changes
watch *args="": hpack
  #!/usr/bin/env bash

  ghcid --command "cabal repl test:spec" --test ':main {{ args }}' --warnings --reload=ts

fileserver *args="":
  nix run .#fileserver -- {{ args }}

run-garner example *args="": hpack
  #!/usr/bin/env bash

  set -eux

  cd examples/{{ example }}
  cabal run garner:garner -- {{ args }}

check-examples:
  just run-garner haskell check haskellExecutable
  just run-garner haskell run haskellExecutable
  echo "node --version" | just run-garner npm-frontend enter frontend
  just run-garner frontend-create-react-app check main
  just run-garner frontend-yarn-webpack check frontend
  just run-garner go-http-backend check server

codegen: hpack && typescript-check
  cabal run codegen

typescript-check *args="":
  deno check ts/*.ts {{ args }}

check-isolated-garner:
  test/check-isolated-garner.sh

# Show the available commands
list:
  just --list

# Check what we can before pushing changes
pre-push: fmt github-ci check-examples

# Run checks that we can’t yet run via the flake
github-ci: test codegen check-isolated-garner run-example-ci test-ts

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

  ghcid --command "cabal repl test:spec" --test ':main {{ args }}' --warnings

fileserver *args="":
  nix run .#fileserver -- {{ args }}

run-garner example *args="": hpack
  #!/usr/bin/env bash

  set -eux

  cd examples/{{ example }}
  cabal run garner:garner -- {{ args }}

run-garner-and-ci example target:
  just run-garner {{ example }} ci {{ target }}

run-example-ci:
  just run-garner-and-ci frontend-create-react-app main
  just run-garner-and-ci frontend-yarn-webpack frontend
  just run-garner-and-ci go-http-backend server

check-examples:
  just run-garner haskell run haskellExecutable
  echo "node --version" | just run-garner npm-frontend enter frontend

codegen: hpack && typescript-check
  cabal run codegen

typescript-check *args="":
  deno check ts/*.ts {{ args }}

check-isolated-garner:
  test/check-isolated-garner.sh

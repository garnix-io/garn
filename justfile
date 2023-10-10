# Show the available commands
list:
  just --list

# Check what we can before pushing changes
pre-push: fmt github-ci

fast-pre-push: fmt test check-examples typescript-check test-ts

# Run checks that we can’t yet run via the flake
github-ci: test codegen check-isolated-garn check-examples test-ts

fmt: fmt-nix fmt-haskell fmt-typescript

check: fmt-nix-check fmt-haskell-check hpack-check fmt-typescript-check

# Push the current version of the ts files to gh-pages
release-ts: codegen
 #!/usr/bin/env bash
 set -eux
 read -p "Release tag (e.g. v0.1.0): " tag
 git tag $tag
 git checkout gh-pages
 mkdir -p tmp-$tag
 git --work-tree=tmp-$tag checkout main -- ts
 mv tmp-$tag/* ts/$tag
 mv ts/nixpkgs.ts ts/$tag
 rmdir tmp-$tag
 git add ts/$tag
 git commit -m "Release $tag"
 git push && git push --tags

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
    oldCabal <- readFile "garn.cabal"
    newCabal <- readProcess "hpack" (words "-") ""
    when (oldCabal /= newCabal) $
      error "package.yaml has changed, please run hpack"

test-ts:
  deno test --allow-write --allow-read --allow-run ts/*.ts

test *args="": hpack
  cabal run --test-show-details=streaming garn:spec -- {{ args }}

# Run the tests continuously as the code changes
watch *args="": hpack
  #!/usr/bin/env bash

  ghcid --command "cabal repl test:spec" --test ':main {{ args }}' --warnings --reload=ts

fileserver *args="":
  nix run .#fileserver -- {{ args }}

run-garn example *args="": hpack
  #!/usr/bin/env bash

  set -eux

  cd examples/{{ example }}
  cabal run garn:garn -- {{ args }}

check-examples:
  just run-garn haskell check helloFromHaskell
  just run-garn haskell run helloFromHaskell
  echo "node --version" | just run-garn npm-frontend enter frontend
  just run-garn frontend-create-react-app check main
  just run-garn frontend-yarn-webpack check frontend
  just run-garn go-http-backend check server

codegen: hpack && typescript-check
  cabal run codegen

typescript-check *args="":
  deno check ts/*.ts {{ args }}

check-isolated-garn:
  test/check-isolated-garn.sh

# Start the docs website server
docs-server:
  cd website && npm install
  cd website && npm run dev

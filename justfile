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

# Deploy the current website
deploy-website commit: build-install-script
  #!/usr/bin/env bash
  set -eux

  TMP_DIR=$(mktemp --directory)
  DIST_DIR=$TMP_DIR/dist
  mkdir -p $DIST_DIR
  cd $TMP_DIR
  git clone git@github.com:garnix-io/garn
  cd garn
  git reset --hard {{ commit }}
  COMMIT=$(git rev-parse HEAD)
  (cd website && npm install && npm run build)
  ls -la website/dist
  mv website/dist/* $DIST_DIR
  git checkout gh-pages
  cp -rv $DIST_DIR/* .
  git add .
  git commit -m "Website update from commit $COMMIT"
  echo "Created a new commit in $TMP_DIR/garn . It is not yet pushed."

# Push the current version of the ts files to gh-pages
release-ts commit:
  #!/usr/bin/env bash
  set -eux

  read -p "Release tag (e.g. v0.1.0): " tag
  TMP_DIR=$(mktemp --directory)
  cd $TMP_DIR
  git clone git@github.com:garnix-io/garn
  cd garn
  git reset --hard {{ commit }}
  git tag $tag

  just codegen

  git checkout gh-pages
  mkdir -p tmp-$tag
  git --work-tree=tmp-$tag checkout {{ commit }} -- ts
  mv tmp-$tag/ts ts/$tag
  mv ts/nixpkgs.ts ts/$tag
  rmdir tmp-$tag
  git add ts/$tag
  git commit -m "Release $tag"
  echo "Created a new commit in $TMP_DIR/garn . It is not yet pushed."
  echo "Created a new tag ($tag) too. Also not yet pushed."

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

  export DISABLE_TTY_TEST=true
  ghcid --command "cabal repl test:spec" --test ':main {{ args }}' --warnings --reload=ts

fileserver *args="":
  nix run .#fileserver -- {{ args }}

run-garn example *args="": hpack
  #!/usr/bin/env bash

  set -eux

  cd examples/{{ example }}
  cabal run garn:garn -- {{ args }}

check-examples:
  just run-garn haskell check
  just run-garn haskell run helloFromHaskell
  just run-garn frontend-create-react-app check
  rm examples/frontend-create-react-app/result
  just run-garn frontend-create-react-app build bundle
  ls examples/frontend-create-react-app/result/index.html
  just run-garn frontend-yarn-webpack check
  just run-garn go-http-backend check
  just run-garn monorepo check

codegen: hpack && typescript-check
  cabal run codegen

typescript-check *args="":
  deno check ts/*.ts {{ args }}
  echo checked!

check-isolated-garn:
  test/check-isolated-garn.sh

# Start the docs website server
docs-server: build-install-script
  cd website && npm install
  cd website && npm run dev

build-install-script:
  nix build -L .#installScript
  mkdir -p website/public
  cat result > website/public/install.sh

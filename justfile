fmt: fmt-nix fmt-haskell

fmt-nix:
  nix fmt

check: fmt-nix-check fmt-haskell-check hpack-check

fmt-nix-check:
  nixpkgs-fmt --check .

fmt-haskell:
  just ormolu inplace

fmt-haskell-check:
  just ormolu check

ormolu mode:
  #!/usr/bin/env bash

  cd backend
  ormolu \
    --cabal-default-extensions \
    --mode {{ mode }} \
    $(find . -name '*.hs' | grep -v '^./dist-newstyle/')

hpack:
  hpack

hpack-check:
  #!/usr/bin/env runhaskell

  import Control.Monad
  import System.Process

  main = do
    oldCabal <- readFile "backend/garnix.cabal"
    newCabal <- readProcess "hpack" (words "backend -") ""
    when (oldCabal /= newCabal) $
      error "package.yaml has changed, please run hpack"


watch *args="": hpack
  #!/usr/bin/env bash

  ghcid --command "cabal repl test:spec" --test ':main {{ args }}' --warnings

example *args="run garnerTest":
  #!/usr/bin/env bash

  set -eux

  cd examples
  nix -L run ..#default -- {{ args }}

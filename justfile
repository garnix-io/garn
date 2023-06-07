check: hpack fmt test

fmt: fmt-nix fmt-haskell fmt-typescript

fmt-nix:
  nixpkgs-fmt *.nix

fmt-haskell:
  #!/usr/bin/env bash

  ormolu \
    --mode inplace \
    $(find . -name '*.hs' | grep -v '^./dist-newstyle/')

fmt-typescript:
  prettier --write 'ts/**/*.ts'

hpack:
  hpack

test: hpack
  cabal test

watch *args="": hpack
  #!/usr/bin/env bash

  ghcid --command "cabal repl test:spec" --test ':main {{ args }}' --warnings

example *args="run garnerTest":
  #!/usr/bin/env bash

  set -eux

  cd examples
  nix -L run ..#default -- {{ args }}

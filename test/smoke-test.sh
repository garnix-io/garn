#!/usr/bin/env bash

set -euo pipefail
shopt -s inherit_errexit

codegen
just fileserver &
sleep 5

cd examples/haskell
expected=42
res="$(garner run haskellExecutable)"
if [[ $res != $expected ]]; then
  echo "Garner ran, but gave a different result than expected."
  echo "  Expected: $expected"
  echo "  Received: $res"
  exit 1
fi

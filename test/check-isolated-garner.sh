#!/usr/bin/env bash

set -euo pipefail
shopt -s inherit_errexit

PROJECT_ROOT="$(git rev-parse --show-toplevel)"

function run-in-isolation () {
  nix shell --ignore-environment "$PROJECT_ROOT#garner" -c "$@"
}

## verify isolation is working
if run-in-isolation git rev-parse --show-toplevel 2>/dev/null; then
  echo "Error: Isolation is broken"
  exit 1
fi

echo running codegen...
run-in-isolation codegen
fs_proc="$("$PROJECT_ROOT/scripts/fileserver" >/dev/null & jobs -p)"
sleep 2
trap "kill -s SIGTERM $fs_proc" EXIT

cd examples/haskell
expected=42
res="$(run-in-isolation garner run haskellExecutable)"
if [[ $res == $expected ]]; then
  echo "Smoke test successful"
else
  echo "Garner ran, but gave a different result than expected."
  echo "  Expected: $expected"
  echo "  Received: $res"
  exit 1
fi

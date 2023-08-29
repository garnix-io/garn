#!/usr/bin/env bash

set -euo pipefail
shopt -s inherit_errexit

PROJECT_ROOT="$(git rev-parse --show-toplevel)"

codegen
fs_proc="$("$PROJECT_ROOT/scripts/fileserver" >/dev/null & jobs -p)"
sleep 2
trap "kill -s SIGTERM $fs_proc" EXIT

cd examples/haskell
expected=42
res="$(garner run haskellExecutable)"
if [[ $res == $expected ]]; then
  echo "Smoke test successful"
else
  echo "Garner ran, but gave a different result than expected."
  echo "  Expected: $expected"
  echo "  Received: $res"
  exit 1
fi

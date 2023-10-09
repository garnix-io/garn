#!/usr/bin/env bash

set -euo pipefail
shopt -s inherit_errexit

PROJECT_ROOT="$(git rev-parse --show-toplevel)"

nix build --print-build-logs \
  "$PROJECT_ROOT#garn"
function run-in-isolation () {
  nix shell --ignore-environment --print-build-logs \
    "$PROJECT_ROOT#garn" -c "$@"
}

## verify isolation is working
if run-in-isolation git --version 2>/dev/null; then
  echo "Error: Isolation is broken"
  exit 1
fi

echo running codegen...
run-in-isolation codegen
nix build --print-build-logs "$PROJECT_ROOT#fileserver"
nix run "$PROJECT_ROOT#fileserver" &
fileserver_pid="$!"
trap "kill -s SIGTERM $fileserver_pid" EXIT
while ! nc localhost 8777 -z; do
  sleep 1
done

cd examples/haskell
expected=42
res="$(run-in-isolation garn run helloFromHaskell)"
if [[ $res == $expected ]]; then
  echo "Smoke test successful"
else
  echo "Garn ran, but gave a different result than expected."
  echo "  Expected: $expected"
  echo "  Received: $res"
  exit 1
fi

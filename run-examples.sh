#!/usr/bin/env bash
# Build and run the PAL examples.
#
# Usage:
#   ./run-examples.sh                  # run every example (results only)
#   ./run-examples.sh dsl              # run a group: code | data | dsl | file
#   ./run-examples.sh data/pairs       # run a single example
#   ./run-examples.sh --trace dsl/maybe  # full Debug trace with context dumps
#   ./run-examples.sh --list           # list available examples
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

# Regenerate pal.cabal if package.yaml changed (hpack is optional).
if command -v hpack >/dev/null 2>&1; then
  hpack >/dev/null
fi

cabal build -v0 exe:pal-examples
exec cabal run -v0 exe:pal-examples -- "$@"

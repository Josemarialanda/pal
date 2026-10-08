#!/usr/bin/env bash
# Build and start the interactive PAL REPL.
#
# Usage:
#   ./pal-repl.sh                                # empty REPL
#   ./pal-repl.sh examples/programs/stlc.pal     # load files first, then REPL
#
# Files are run in order in one context before the prompt appears
# (the same as `pal -i FILE...`). Relative paths are resolved from the
# directory you run the script in, not from the project root.
set -euo pipefail

project_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Build from the project root, but keep the caller's working directory
# so relative file arguments still point where the user meant.
pal_bin="$(
  cd "$project_dir"
  # Regenerate pal.cabal if package.yaml changed (hpack is optional).
  if command -v hpack >/dev/null 2>&1; then
    hpack >/dev/null
  fi
  cabal build -v0 exe:pal >&2 || exit 1
  cabal list-bin -v0 exe:pal
)"

if [ "$#" -eq 0 ]; then
  exec "$pal_bin"
else
  exec "$pal_bin" --interactive "$@"
fi

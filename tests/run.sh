#!/usr/bin/env bash
# Run PAL's regression tests.
#
#   tests/run.sh             run every test
#   tests/run.sh --accept    also rewrite tests/fail/*.out from the current output
#
# Uses $PAL as the pal executable (default: `pal` on PATH). Run it from the
# project root; the dev shell's `run-tests` command does this for you.
#
#   tests/pass/*.pal          must succeed: every definition accepted, every
#                             infer typechecks, every check and fails met
#   examples/programs/*.pal   likewise (except broken.pal, a parse error on purpose)
#   tests/fail/*.pal          must fail, with exactly the output in the
#                             matching .out file (messages, locations, context)
set -uo pipefail

pal="${PAL:-pal}"
accept=false
[ "${1:-}" = "--accept" ] && accept=true

passed=0
failed=0
ok() { echo "✓ $1"; passed=$((passed + 1)); }
bad() { echo "✗ $1"; failed=$((failed + 1)); }

for f in tests/pass/*.pal examples/programs/*.pal; do
  [ "$f" = examples/programs/broken.pal ] && continue
  if out=$(NO_COLOR=1 "$pal" "$f" 2>&1); then
    ok "$f"
  else
    bad "$f (expected success)"
    printf '%s\n' "$out" | sed 's/^/    /'
  fi
done

for f in tests/fail/*.pal; do
  expected="${f%.pal}.out"
  if out=$(NO_COLOR=1 "$pal" "$f" 2>&1); then
    bad "$f (expected failure)"
    continue
  fi
  if $accept; then
    printf '%s\n' "$out" > "$expected"
    ok "$f (accepted)"
  elif diff -u "$expected" <(printf '%s\n' "$out") > /dev/null 2>&1; then
    ok "$f"
  else
    bad "$f (output differs from $expected)"
    diff -u "$expected" <(printf '%s\n' "$out") | sed 's/^/    /'
  fi
done

echo
if [ "$failed" -eq 0 ]; then
  echo "$passed passed"
else
  echo "$failed of $((passed + failed)) failed"
  exit 1
fi

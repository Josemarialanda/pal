#!/usr/bin/env bash
# Assemble a Vercel Build Output API (v3) directory for the PAL web UI.
#
#   deploy/vercel/build-output.sh PAL_UI_BINARY [OUT_DIR]
#
# OUT_DIR defaults to .vercel/output. The result is deployed with
# `vercel deploy --prebuilt`:
#
#   static/index.html            the page (ui/static/index.html)
#   functions/api/run.func/      a Node function that runs pal-ui (see run.js)
#
# pal-ui is copied with every shared library it links and the dynamic
# loader, and run.js starts it through that loader, so it doesn't depend
# on the libraries installed on Vercel's machines.
set -euo pipefail

bin="${1:?usage: build-output.sh PAL_UI_BINARY [OUT_DIR]}"
out="${2:-.vercel/output}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
fn="$out/functions/api/run.func"

rm -rf "$out"
mkdir -p "$out/static" "$fn/api" "$fn/pal/lib"

cp "$root/ui/static/index.html" "$out/static/index.html"
cp "$here/run.js" "$fn/api/run.js"

cp "$bin" "$fn/pal/pal-ui"
chmod u+w "$fn/pal/pal-ui"
strip "$fn/pal/pal-ui" 2>/dev/null || true
ldd "$bin" | awk '/=> \// { print $3 }' | xargs -I{} cp -L {} "$fn/pal/lib/"
loader="$(ldd "$bin" | awk '/ld-linux/ { print $1 }')"
cp -L "$loader" "$fn/pal/lib/ld-linux-x86-64.so.2"
chmod -R u+w "$fn/pal"

cat >"$fn/.vc-config.json" <<'JSON'
{
  "runtime": "nodejs22.x",
  "handler": "api/run.js",
  "launcherType": "Nodejs",
  "shouldAddHelpers": false,
  "maxDuration": 30,
  "memory": 1024
}
JSON
cat >"$out/config.json" <<'JSON'
{ "version": 3, "routes": [{ "handle": "filesystem" }] }
JSON

# Fail now, not on Vercel, if the bundle can't start without the host's libraries.
env -i "$fn/pal/lib/ld-linux-x86-64.so.2" --library-path "$fn/pal/lib" "$fn/pal/pal-ui" --help >/dev/null
echo "Wrote $out ($(du -sh "$out" | cut -f1))"

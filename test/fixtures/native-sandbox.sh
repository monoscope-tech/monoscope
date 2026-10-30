#!/bin/sh
set -eu
if [ "$(uname -s)" = Darwin ]; then
  export GONDOLIN_VMM="${GONDOLIN_VMM:-krun}"
fi
exec "$(dirname "$0")/../../impact-review-worker/sandbox.mjs"

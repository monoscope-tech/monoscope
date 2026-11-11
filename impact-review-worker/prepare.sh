#!/bin/bash
# Warms a repository's build at its pull request's base, offline, in the review guest. Build output
# stays in the checkout's ignored directories (dist-newstyle/, target/, node_modules/), so proofs at the
# head compile only the change. A stack that cannot build here is skipped: the review just starts cold.
set -u
export CARGO_HOME=$HOME/.cargo GOPROXY=off
try() { echo "== $*"; "$@" || echo "!! $1 failed; reviews of this base start cold"; }

if [ -f web-components/vite.config.mjs ] && [ -d /opt/proof/node_modules ]; then
  ln -sfn /opt/proof/node_modules node_modules
  ln -sfn /opt/proof/web-components/node_modules web-components/node_modules
  # Template Haskell reads the bundle manifest, so the frontend builds before the server.
  try ./node_modules/.bin/tailwindcss -i static/public/assets/css/tailwind.css -o static/public/assets/css/tailwind.min.css
  (cd web-components && try ./node_modules/.bin/vite build)
fi
if [ -f cabal.project ] && command -v cabal >/dev/null; then
  if [ -f monoscope.cabal ]; then
    # Its git-sourced dependencies were fetched when the guest was built; the VM has no network.
    mkdir -p dist-newstyle/src && cp -Rn /opt/haskell/dist-src/. dist-newstyle/src/ 2>/dev/null
    try cabal build exe:monoscope-server --offline --ghc-options="-O0 -j2"
  else try cabal build all --offline --ghc-options=-O0; fi
fi
[ -f Cargo.lock ] && command -v cargo >/dev/null && try cargo build --offline --locked
[ -f go.sum ] && command -v go >/dev/null && try go build ./...
exit 0

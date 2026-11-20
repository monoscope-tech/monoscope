#!/bin/bash
# Warms a repository's build at a review's or fix's base in the guest, for whatever stack it uses.
# Dependencies come from the package registries the sandbox allows; build output stays in the
# checkout's ignored directories (dist-newstyle/, target/, node_modules/, .venv/), and tests are built
# too, so a fix's regression test and the review's proofs compile only the change. A stack whose tools
# are missing or whose build fails is skipped: that base just starts cold.
set -u
export CARGO_HOME=$HOME/.cargo
try() { echo "== $*"; "$@" || echo "!! $1 failed; work at this base starts cold"; }
has() { command -v "$1" >/dev/null; }

if [ -f web-components/vite.config.mjs ] && [ -d /opt/proof/node_modules ]; then
  ln -sfn /opt/proof/node_modules node_modules
  ln -sfn /opt/proof/web-components/node_modules web-components/node_modules
  # Template Haskell reads the bundle manifest, so the frontend builds before the server.
  try ./node_modules/.bin/tailwindcss -i static/public/assets/css/tailwind.css -o static/public/assets/css/tailwind.min.css
  (cd web-components && try ./node_modules/.bin/vite build)
elif [ -f package.json ]; then
  if [ -f pnpm-lock.yaml ] && has pnpm; then try pnpm install --frozen-lockfile
  elif [ -f yarn.lock ] && has yarn; then try yarn install --frozen-lockfile
  elif has npm; then if [ -f package-lock.json ]; then try npm ci; else try npm install; fi; fi
  grep -q '"build"' package.json && has npm && try npm run build --if-present
fi

if [ -f cabal.project ] && has cabal; then
  if [ -f monoscope.cabal ]; then
    # Its git-sourced dependencies were fetched when the guest was built; git hosts are not reachable.
    mkdir -p dist-newstyle/src && cp -Rn /opt/haskell/dist-src/. dist-newstyle/src/ 2>/dev/null
    # The configuration lives in cabal.project.local, not on the command line, so any later plain
    # `cabal build`/`cabal test` here (the agent's checks) reuses this build instead of reconfiguring.
    printf 'tests: True\noptimization: False\npackage monoscope\n  ghc-options: -j4\n' > cabal.project.local
    try cabal build exe:monoscope-server test:integration-tests --offline
  else try cabal update && try cabal build all --enable-tests --ghc-options=-O0; fi
elif [ -f stack.yaml ] && has stack; then try stack build --test --no-run-tests --fast
fi

[ -f Cargo.toml ] && has cargo && try cargo test --no-run --locked
[ -f go.mod ] && has go && try go test -run '^$' ./...

if [ -f uv.lock ] && has uv; then try uv sync --all-extras
elif [ -f poetry.lock ] && has poetry; then try poetry install --no-interaction
elif has python3 && { [ -f requirements.txt ] || [ -f pyproject.toml ] || [ -f setup.py ]; }; then
  try python3 -m venv .venv
  [ -f requirements.txt ] && try .venv/bin/pip install -r requirements.txt
  [ -f requirements-dev.txt ] && try .venv/bin/pip install -r requirements-dev.txt
  { [ -f pyproject.toml ] || [ -f setup.py ]; } && try .venv/bin/pip install -e .
fi

if [ -f pom.xml ] && has mvn; then try mvn -q -B test-compile
elif { [ -f build.gradle ] || [ -f build.gradle.kts ]; }; then
  if [ -x ./gradlew ]; then try ./gradlew --no-daemon testClasses; elif has gradle; then try gradle --no-daemon testClasses; fi
fi

[ -f Gemfile ] && has bundle && try bundle install
exit 0

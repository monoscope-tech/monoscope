# Single source of truth for GHC version: cabal.project's `with-compiler: ghc-X.Y.Z`.
# Dockerfile.deps and .github/workflows/cli-release.yml must be kept in sync manually.
GHC_VERSION := $(shell awk '/^with-compiler:[[:space:]]*ghc-/ {sub(/^ghc-/, "", $$2); print $$2}' cabal.project)
GHC := ghc-$(GHC_VERSION)
ARCH := $(shell uname -m | sed 's/arm64/aarch64/' | tr '[:upper:]' '[:lower:]')
OS := $(shell uname -s | sed 's/Darwin/osx/' | tr '[:upper:]' '[:lower:]')
OS_ARCH := $(ARCH)-$(OS)

# Detect number of CPU cores (works on macOS and Linux)
NCPUS := $(shell sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4)

# App port the live-reload server binds (matches System.Config default; override
# to match a custom PORT in your .env).
PORT ?= 8080

# Built bundles whose content is baked into the page at COMPILE time via
# hashAssetFile's TH splice (see BodyWrapper). They're addDependentFile deps, so
# having ghcid watch them makes a JS/CSS-only rebuild trigger a :reload: GHC
# re-runs the splice, the ?v= cache-buster updates, and the browser fetches the
# freshly-built bundle instead of a stale cached copy. Without this, a JS change
# never bumps the hash (ghcid only watches .hs) so the old bundle keeps serving.
RELOAD_ASSETS := --reload=static/public/assets/web-components/dist/js/index.js \
                 --reload=static/public/assets/web-components/dist/css/index.css \
                 --reload=static/public/assets/css/tailwind.min.css

css-start:
	./node_modules/.bin/tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --watch 2>&1 | tee css.log
post-css:
	./node_modules/.bin/tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css

web-components-watch:
	cd web-components && npm run watch 2>&1 | tee ../web-components.log
run:
	cabal run

# CYPRESS_RECORD_KEY comes from .env (Cypress reads it natively)
cypress:
	set -a && . ./.env && npx cypress run --record

# Kill any prior live-reload app + its ghcid/cabal-repl/ghci tree by the PROCESS
# GROUP of whatever holds $(PORT). Group-killing is what avoids the orphaned
# `cabal repl` processes + stale :8080 that pile up when ghcid is C-c'd: SIGINT
# doesn't reliably reach the GHCi-hosted Warp server, so the old app keeps the
# port and each restart's `:run Start.startApp` silently fails to rebind. Safe
# by construction — the port owner is in a different process group than this
# make invocation, so we never kill ourselves.
kill-live-reload:
	@PID=$$(lsof -ti tcp:$(PORT) 2>/dev/null | head -1); \
	if [ -n "$$PID" ]; then \
		PGID=$$(ps -o pgid= -p $$PID 2>/dev/null | tr -d ' '); \
		if [ -n "$$PGID" ]; then \
			echo "Freeing :$(PORT) — killing process group $$PGID"; \
			kill -TERM -$$PGID 2>/dev/null || true; \
			sleep 1; \
		fi; \
		REMAIN=$$(lsof -ti tcp:$(PORT) 2>/dev/null); \
		[ -n "$$REMAIN" ] && kill -9 $$REMAIN 2>/dev/null || true; \
	else \
		echo "Port :$(PORT) already free"; \
	fi

live-reload: kill-live-reload
	ghcid --command 'cabal repl monoscope --no-semaphore --ghc-options="-j$(NCPUS) -Wno-error=unused-imports -Wno-error=unused-top-binds" --with-compiler=$(GHC)' --test ':run Start.startApp' $(RELOAD_ASSETS) --warnings

live-reload-cli:
	ghcid --command 'cabal repl exe:monoscope --no-semaphore --ghc-options="-O0 -Wno-error=unused-imports -Wno-error=unused-top-binds" --with-compiler=$(GHC)' --warnings 2>&1 | tee build-cli.log

live-test-reload:
	ghcid --command 'cabal repl lib:monoscope test/unit/Main.hs --no-semaphore --with-compiler=$(GHC)' --test ':run main' --warnings

live-test-reload-unit:
	ghcid --test 'cabal test monoscope:unit-tests --test-show-details=streaming'

live-test-reload-all:
	ghcid --test 'cabal test monoscope:tests --test-show-details=streaming'

# Integration tests with lib+test in ONE GHCi target (Jade's "cabal test-dev" trick).
# `:reload` crosses src/<->test/ boundaries — no relink between iterations.
# `-osuf dyn_o -hisuf dyn_hi` reuses the .dyn_o artifacts cabal already wrote, so
# the cold load is fast. `-fobject-code` is the reliability half: without it GHCi
# recompiles CHANGED modules to bytecode while unchanged ones stay object code,
# and a bytecode module referencing an object module's constructor info-table
# (`..._con_info`) fails macOS flat-namespace resolution in dynLoadObjs — the
# intermittent "symbol not found in flat namespace" relink flake. Forcing every
# recompile to object code keeps the whole session single-mode, so it can't mix.
# Filter with: TEST_MATCH=Monitoring make live-test-dev
# (hspec-discover node names drop the module's "Spec" suffix)
# NB: GHCi's :main only strips quotes at token start, so `--match="X"` would
# pass the quote chars to hspec and silently match nothing — keep the space form.
TEST_MATCH ?=
live-test-dev:
	USE_EXTERNAL_DB=true LOG_LEVEL=attention \
	ghcid --command 'cabal repl monoscope:test:test-dev --no-semaphore --ghc-options="-j$(NCPUS) -fobject-code -osuf dyn_o -hisuf dyn_hi -O0" --with-compiler=$(GHC)' \
		--test ':main $(if $(TEST_MATCH),--match $(TEST_MATCH))' --warnings 2>&1 | tee build-test-dev.log

hot-reload:
	livereload -f reload.trigger static/public/ & \
	ghcid --command 'cabal repl --no-semaphore' --test ':run Start.startApp' --test ':! (sleep 1 && touch static/public/reload.trigger)'  --warnings

watch:
	# https://github.com/MercuryTechnologies/ghciwatch/issues/143
	# GHCI currently doesnt support non-terminating test actions like webservers.
	# So it should be used only for checking compile time and generating static-ls actions
	# And for repeatedly running tests on code changes
	# ghciwatch --test-ghci Start.startApp --error-file errors.err  --before-startup-shell hpack --clear  --watch
	ghciwatch --error-file errors.err  --before-startup-shell hpack --clear  --watch


live-test-reload-cabal:
	ghcid --test 'cabal test --test-show-details=streaming'

# `make test` = the fast default: PROCESS-level sharded integration suite. In-process
# hspec `parallel` deadlocks on the per-test resource-pool lifecycle (see
# test/integration/Main.hs), so we run SHARDS copies of the test binary concurrently —
# each its own RTS running a disjoint shard SEQUENTIALLY (no shared state to race).
# Fast by default; no env/params to remember.
test: test-shards

# Build once, then run SHARDS shard processes concurrently. Each shard keeps ~1 test DB
# live, so SHARDS * ~9 connections must stay under Postgres max_connections — 6 is safe
# on a stock (100) server; bump SHARDS with the tmpfs DB (`make timescaledb-docker-tmp`,
# max_connections=200). Per-shard output: build-shard-<i>.log.
SHARDS ?= 6
test-shards:
	cabal build integration-tests --ghc-options="-O0 +RTS -A64m -RTS"
	@bin=$$(cabal list-bin integration-tests); echo "sharding $(SHARDS)x: $$bin"; \
	rm -f build-shard-*.log; \
	for i in $$(seq 0 $$(( $(SHARDS) - 1 ))); do \
	  ( start=$$(date +%s); SHARD_INDEX=$$i SHARD_TOTAL=$(SHARDS) USE_EXTERNAL_DB=true LOG_LEVEL=attention \
	    $$bin --color > build-shard-$$i.log 2>&1; echo "[shard-time] $$(( $$(date +%s) - start ))s" >> build-shard-$$i.log ) & \
	done; wait; \
	echo "=== per-shard (wall-clock | result) — wide spreads ⇒ rebalance ==="; \
	for i in $$(seq 0 $$(( $(SHARDS) - 1 ))); do \
	  printf "shard %s: %-6s | %s\n" "$$i" "$$(sed -n 's/.*\[shard-time\] //p' build-shard-$$i.log | tail -1)" "$$(grep -hE 'examples?, [0-9]+ failures?' build-shard-$$i.log | tail -1)"; \
	done; \
	green=$$(grep -lE "examples?, 0 failures" build-shard-*.log | wc -l | tr -d ' '); \
	if [ "$$green" -ne $(SHARDS) ] || grep -qE "[1-9][0-9]* failures?|error:|Interrupted" build-shard-*.log; then \
	  echo "SHARDED RUN FAILED ($$green/$(SHARDS) shards green) — failing shard output:"; \
	  for f in build-shard-*.log; do grep -qE "examples?, 0 failures" "$$f" || { echo "### $$f"; grep -E "✘| [0-9]+\) |error:|but got|expected|Interrupted" "$$f" | head -20; }; done; exit 1; \
	else echo "ALL SHARDS GREEN ($$green/$(SHARDS))"; fi

test-unit:
	cabal test unit-tests -j --ghc-options="-O0"  --test-show-details=direct --test-options='--color --jobs=$(NCPUS)'

test-doctests:
	cabal test doctests -j --ghc-options="-O0" --test-show-details=direct

test-integration:
	LOG_LEVEL=attention USE_EXTERNAL_DB=true cabal test integration-tests -j --ghc-options="-O0" --test-show-details=direct --test-options='--color --jobs=$(NCPUS)'

# TimeFusion bring-up for integration tests. Delegates to the timefusion repo.
# Override TIMEFUSION_DIR if your checkout lives elsewhere.
TIMEFUSION_DIR ?= ../timefusion
TIMEFUSION_PG_TEST_URL ?= postgresql://postgres:postgres@localhost:12345/postgres

timefusion-start:
	$(MAKE) -C $(TIMEFUSION_DIR) tf-start

timefusion-stop:
	$(MAKE) -C $(TIMEFUSION_DIR) tf-stop

# Run integration tests against a real TimeFusion (started + stopped automatically).
test-integration-tf: timefusion-start
	@rc=0; \
	TIMEFUSION_PG_TEST_URL=$(TIMEFUSION_PG_TEST_URL) LOG_LEVEL=attention USE_EXTERNAL_DB=true \
		cabal test integration-tests -j --ghc-options="-O0" --test-show-details=direct --test-options='--color --jobs=$(NCPUS)' || rc=$$?; \
	$(MAKE) timefusion-stop; \
	exit $$rc

test-collector-tier1:
	./test/collector/run.sh

test-collector-drift:
	./test/collector/check-drift.sh

bench-collector-tier1:
	./test/collector/bench.sh

live-test-unit:
	ghcid --test 'cabal test monoscope:unit-tests --test-show-details=streaming'

live-reload-doctests:
	ghcid --command 'cabal repl lib:monoscope --no-semaphore --with-compiler=$(GHC)' --test ':! cabal test monoscope:doctests --ghc-options="-O0" --test-show-details=streaming'

fmt:
	fourmolu --mode inplace $$(find ./src/ -name '*.hs')

fix-imports:
	fix-imports $$(find ./src -name '*.hs') <$$(find ./src -name '*.hs')

lint:
	hlint src

fix-lint:
	find ./src -name '*.hs' | xargs -L1 hlint --refactor --refactor-options="--inplace"

# Add a FontAwesome icon to the sprite sheet.
# Usage: make fa-add ICON=arrow-right STYLE=solid
#        make fa-add ICON=circle-check STYLE=regular
fa-add:
	@test -n "$(ICON)" || (echo "usage: make fa-add ICON=icon-name STYLE=solid|regular"; exit 1)
	python3 scripts/fa-add.py $(ICON) $${STYLE:-regular}

gen-proto:
	cd proto && protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --haskell_out=. \
    --proto_path=. \
    $$(find opentelemetry -name '*.proto')

# Sync proto/opentelemetry/ from upstream open-telemetry/opentelemetry-proto.
# Defaults to the latest release tag; override with `make sync-otel-proto OTEL_PROTO_REF=main`.
# Only the `opentelemetry/` tree is copied; nothing else from upstream lands here.
sync-otel-proto:
	@set -euo pipefail; \
	REF=$${OTEL_PROTO_REF:-$$(git ls-remote --tags --refs https://github.com/open-telemetry/opentelemetry-proto.git \
		| awk -F/ '{print $$NF}' | grep -E '^v[0-9]+\.[0-9]+\.[0-9]+$$' | sort -V | tail -1)}; \
	echo "syncing opentelemetry-proto @ $$REF"; \
	TMP=$$(mktemp -d); trap "rm -rf $$TMP" EXIT; \
	git -C $$TMP clone --depth=1 --branch=$$REF https://github.com/open-telemetry/opentelemetry-proto.git src >/dev/null 2>&1; \
	rm -rf proto/opentelemetry; \
	mkdir -p proto/opentelemetry; \
	cp -R $$TMP/src/opentelemetry/. proto/opentelemetry/; \
	echo "$$REF" > proto/opentelemetry/.version; \
	echo "wrote proto/opentelemetry/.version=$$REF — run 'make gen-proto && hpack' next"

# Convenience alias — Haskell bindings are regenerated automatically by
# proto-lens-setup at cabal build time, so syncing the .proto files is enough.
update-otel-proto: sync-otel-proto

timescaledb-docker:
	docker run -it --rm --name=monoscope -p 5432:5432/tcp -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=monoscope -v $$HOME/pg-data:/home/postgres/pgdata \
		docker.io/timescale/timescaledb-ha:pg16-all -c shared_preload_libraries='pg_stat_statements,timescaledb'

timescaledb-docker-tmp:
	docker run -it --rm --name=monoscope -p 5432:5432/tcp \
		-e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=monoscope \
		-e TZ=UTC \
		--mount type=tmpfs,destination=/home/postgres/pgdata,tmpfs-size=2G \
		--user root \
		--entrypoint /bin/bash \
		docker.io/timescale/timescaledb-ha:pg16-all \
		-c "apt-get update -qq && apt-get install -y -qq faketime > /dev/null 2>&1 && \
		    exec gosu postgres faketime '2025-01-01 01:00:00' docker-entrypoint.sh postgres \
		    -c shared_preload_libraries='pg_stat_statements,timescaledb' \
		    -c max_connections=200"

# Local MinIO for replay/S3 integration tests. Requires the `minio` binary
# (`brew install minio/stable/minio` on macOS, or
# `curl -O https://dl.min.io/server/minio/release/linux-amd64/minio && chmod +x minio`).
# Data dir is gitignored under .local/. Tests probe MINIO_ENDPOINT (default
# http://127.0.0.1:9000) and pendingWith if it's not reachable.
minio-local:
	@mkdir -p .local/minio-data
	@echo "Starting MinIO at http://127.0.0.1:9000 (console http://127.0.0.1:9001)"
	@echo "  access key: minioadmin"
	@echo "  secret key: minioadmin"
	MINIO_ROOT_USER=minioadmin MINIO_ROOT_PASSWORD=minioadmin \
	  minio server .local/minio-data --address ":9000" --console-address ":9001"

update-service-worker:
	npx workbox generateSW config/workbox-config.js

show-os-arch:
	@echo "OS and Architecture: $(OS_ARCH)"

show-ghc-version:
	@echo "GHC Version: $(GHC_VERSION)"


build-chart-cli:
	cd web-components && bun build --compile src/chart-cli.ts --outfile ../chart-cli

build-chart-cli-linux:
	cd web-components && bun build --compile --target=bun-linux-x64 src/chart-cli.ts --outfile ../chart-cli

# Pin a build pane once (`make tmux-pin-here` from the pane you want), then
# every tmux-live-reload* reuses it. Pin survives ghcid restarts because we
# track the pane id in a tmux user option instead of grepping for the command.
define tmux_run
	@PANE=$$(tmux show-option -wv -q @build-pane 2>/dev/null); \
	if [ -n "$$PANE" ] && tmux list-panes -a -F '#{pane_id}' | grep -qx "$$PANE"; then \
		echo "Reusing pinned pane $$PANE"; \
		tmux send-keys -t "$$PANE" C-c; sleep 0.5; \
		tmux send-keys -t "$$PANE" '$(1)' Enter; \
	elif [ -n "$$TMUX" ]; then \
		PANE=$$(tmux split-window -d -h -P -F '#{pane_id}' '$(1)'); \
		tmux set-option -w @build-pane "$$PANE"; \
		echo "Pinned new pane $$PANE (use 'make tmux-unpin' to clear)"; \
	else \
		echo "Not in tmux — running in foreground"; \
		$(1); \
	fi
endef

tmux-pin-here:
	@tmux set-option -w @build-pane "$$TMUX_PANE" && echo "Pinned build pane to $$TMUX_PANE"

tmux-unpin:
	@tmux set-option -wu @build-pane && echo "Unpinned build pane"

tmux-live-reload:
	$(call tmux_run,make live-reload 2>&1 | tee build.log)

tmux-live-reload-cli:
	$(call tmux_run,make live-reload-cli 2>&1 | tee build-cli.log)

e2e-install:
	@test -x e2e/node_modules/.bin/playwright || (cd e2e && npm install && npx playwright install chromium)

test-e2e: e2e-install
	cd e2e && npx playwright test

test-e2e-real: e2e-install
	cd e2e && E2E_REAL_PROVIDERS=true npx playwright test

test-e2e-ui: e2e-install
	cd e2e && npx playwright test --ui

.PHONY: all test fmt lint fix-lint live-reload kill-live-reload live-reload-cli live-reload-doctests live-test-dev build-chart-cli build-chart-cli-linux tmux-live-reload tmux-live-reload-cli tmux-pin-here tmux-unpin web-components-watch e2e-install test-e2e test-e2e-real test-e2e-ui gen-proto sync-otel-proto update-otel-proto minio-local timefusion-start timefusion-stop test-integration-tf

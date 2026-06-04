# Single source of truth for GHC version: cabal.project's `with-compiler: ghc-X.Y.Z`.
# Dockerfile.deps and .github/workflows/cli-release.yml must be kept in sync manually.
GHC_VERSION := $(shell awk '/^with-compiler:[[:space:]]*ghc-/ {sub(/^ghc-/, "", $$2); print $$2}' cabal.project)
GHC := ghc-$(GHC_VERSION)
ARCH := $(shell uname -m | sed 's/arm64/aarch64/' | tr '[:upper:]' '[:lower:]')
OS := $(shell uname -s | sed 's/Darwin/osx/' | tr '[:upper:]' '[:lower:]')
OS_ARCH := $(ARCH)-$(OS)

# Detect number of CPU cores (works on macOS and Linux)
NCPUS := $(shell sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4)

css-start:
	./node_modules/.bin/tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --watch 2>&1 | tee css.log
post-css:
	./node_modules/.bin/tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css

web-components-watch:
	cd web-components && npm run watch 2>&1 | tee ../web-components.log
run:
	cabal run

cypress:
	npx cypress run --record --key 2a2372e2-4ba1-4cd5-8bed-f39f4f047b3e

live-reload:
	ghcid --command 'cabal repl monoscope --ghc-options="-Wno-error=unused-imports -Wno-error=unused-top-binds" --with-compiler=$(GHC)' --test ':run Start.startApp' --warnings

live-reload-cli:
	ghcid --command 'cabal repl exe:monoscope --ghc-options="-O0 -Wno-error=unused-imports -Wno-error=unused-top-binds" --with-compiler=$(GHC)' --warnings 2>&1 | tee build-cli.log

live-test-reload:
	ghcid --command 'cabal repl lib:monoscope test/unit/Main.hs --with-compiler=$(GHC)' --test ':run main' --warnings

live-test-reload-unit:
	ghcid --test 'cabal test monoscope:unit-tests --test-show-details=streaming'

live-test-reload-all:
	ghcid --test 'cabal test monoscope:tests --test-show-details=streaming'

# Integration tests with lib+test in ONE GHCi target (Jade's "cabal test-dev" trick).
# `:reload` crosses src/<->test/ boundaries — no relink between iterations.
# `-osuf dyn_o -hisuf dyn_hi` reuses .dyn_o artifacts cabal already wrote.
# Filter with: TEST_MATCH=/MonitoringSpec/ make live-test-dev
TEST_MATCH ?=
live-test-dev:
	USE_EXTERNAL_DB=true LOG_LEVEL=attention \
	ghcid --command 'cabal repl monoscope:test:test-dev --ghc-options="-osuf dyn_o -hisuf dyn_hi -O0" --with-compiler=$(GHC)' \
		--test ':main $(if $(TEST_MATCH),--match="$(TEST_MATCH)")' --warnings 2>&1 | tee build-test-dev.log

hot-reload:
	livereload -f reload.trigger static/public/ & \
	ghcid --command 'cabal repl' --test ':run Start.startApp' --test ':! (sleep 1 && touch static/public/reload.trigger)'  --warnings

watch:
	# https://github.com/MercuryTechnologies/ghciwatch/issues/143
	# GHCI currently doesnt support non-terminating test actions like webservers.
	# So it should be used only for checking compile time and generating static-ls actions
	# And for repeatedly running tests on code changes
	# ghciwatch --test-ghci Start.startApp --error-file errors.err  --before-startup-shell hpack --clear  --watch
	ghciwatch --error-file errors.err  --before-startup-shell hpack --clear  --watch


live-test-reload-cabal:
	ghcid --test 'cabal test --test-show-details=streaming'

test:
	# --test-show-details=never - Shows only a summary at the end
	# --test-show-details=failures - Shows output only for failed tests (after completion)
	# --test-show-details=always - Shows all test output, but buffers it and displays after the test suite completes
	# --test-show-details=streaming - Similar to direct, provides real-time output (introduced in newer Cabal versions)
	#  -test-show-details=direct Cabal streams the test output directly to your terminal in real-time as the tests run.
	USE_EXTERNAL_DB=true  cabal test -j --ghc-options="-O0"  --test-show-details=never --test-options='--color --jobs=$(NCPUS)'

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
	ghcid --command 'cabal repl lib:monoscope --with-compiler=$(GHC)' --test ':! cabal test monoscope:doctests --ghc-options="-O0" --test-show-details=streaming'

fmt:
	fourmolu --mode inplace $$(find ./src/ -name '*.hs')

fix-imports:
	fix-imports $$(find ./src -name '*.hs') <$$(find ./src -name '*.hs')

lint:
	hlint src

fix-lint:
	find ./src -name '*.hs' | xargs -L1 hlint --refactor --refactor-options="--inplace"

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
		--mount type=tmpfs,destination=/var/lib/postgresql/data,tmpfs-size=1G \
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

.PHONY: all test fmt lint fix-lint live-reload live-reload-cli live-reload-doctests live-test-dev build-chart-cli build-chart-cli-linux tmux-live-reload tmux-live-reload-cli tmux-pin-here tmux-unpin web-components-watch e2e-install test-e2e test-e2e-real test-e2e-ui gen-proto sync-otel-proto update-otel-proto minio-local timefusion-start timefusion-stop test-integration-tf

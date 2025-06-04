# GHC_VERSION := $(shell stack ghc -- --version | awk '{print $$NF}')
# GHC_VERSION := $(shell stack ghc -- --version | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+' | head -n 1)
GHC_VERSION := 9.10.2
ARCH := $(shell uname -m | sed 's/arm64/aarch64/' | tr '[:upper:]' '[:lower:]')
OS := $(shell uname -s | sed 's/Darwin/osx/' | tr '[:upper:]' '[:lower:]')
OS_ARCH := $(ARCH)-$(OS)
LINUX_HC_PATH := .stack-work/dist/x86_64-linux-tinfo6/ghc-$(GHC_VERSION)/build
RUSTLIB := Crust_interop

css-start:
	./node_modules/.bin/tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --watch
post-css:
	./node_modules/.bin/tailwindcss --postcss ./static/public/assets/css/tailwind.css  -o ./static/public/assets/css/tailwind.min.css
run:
	stack run

cypress:
	npx cypress run --record --key 2a2372e2-4ba1-4cd5-8bed-f39f4f047b3e

live-reload:
	# ghcid --command 'stack ghci apitoolkit-server --ghc-options=-w --with-compiler=ghc-9.8.2' --test ':run Start.startApp' --warnings
	# ghcid --command 'stack ghci apitoolkit-server --ghc-options="-w -j4 +RTS -A128m -n2m -RTS"' --test ':run Start.startApp' --warnings
	ghcid --command 'cabal repl --ghc-options="-w -j4 -Wno-error=unused-imports -Wno-error=unused-top-binds" --with-compiler=ghc-9.10.2' --test ':run Start.startApp' --warnings


hot-reload:
	livereload -f reload.trigger static/public/ & \
	ghcid --command 'cabal repl --ghc-options="-w -j4"' --test ':run Start.startApp' --test ':! (sleep 1 && touch static/public/reload.trigger)'  --warnings

watch:
	# https://github.com/MercuryTechnologies/ghciwatch/issues/143
	# GHCI currently doesnt support non-terminating test actions like webservers.
	# So it should be used only for checking compile time and generating static-ls actions
	# And for repeatedly running tests on code changes
	# ghciwatch --test-ghci Start.startApp --error-file errors.err  --before-startup-shell hpack --clear  --watch
	ghciwatch --error-file errors.err  --before-startup-shell hpack --clear  --watch


live-test-reload:
	ghcid --command 'stack ghci apitoolkit-server:apitoolkit-server-test --ghc-options=-w' --test ':run Main.main' --warnings

live-test-reload-stack:
	stack test --ghc-options=-w --file-watch
	# stack test --ghc-options=-w --ta='--match "SeedingConfig/should parse simple config to obj"' --file-watch

test:
	stack test --ghc-options=-w

test-unit:
	stack test apitoolkit-server:unit-tests --ghc-options=-w

live-test-unit:
	stack test apitoolkit-server:unit-tests --ghc-options=-w --file-watch

fmt:
	fourmolu --mode inplace $$(find ./src/ -name '*.hs')

fix-imports:
	fix-imports $$(find ./src -name '*.hs') <$$(find ./src -name '*.hs')

lint:
	hlint src

fix-lint:
	find ./src -name '*.hs' | xargs -L1 hlint --refactor --refactor-options="--inplace"

gen-proto:
	protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --haskell_out=. \
    --proto_path=. \
    opentelemetry/**/*.proto

timescaledb-docker:
	docker run -it --rm --name=apitoolkit -p 5432:5432/tcp -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=apitoolkit -v $$HOME/pg-data:/home/postgres/pgdata \
		docker.io/timescale/timescaledb-ha:pg16-all -c shared_preload_libraries='pg_stat_statements,timescaledb'

timescaledb-docker-tmp:
	docker run -it --rm --name=apitoolkit -p 5432:5432/tcp -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=apitoolkit \
		docker.io/timescale/timescaledb-ha:pg16-all -c shared_preload_libraries='pg_stat_statements,timescaledb'

update-service-worker:
	npx workbox generateSW workbox-config.js

show-os-arch:
	@echo "OS and Architecture: $(OS_ARCH)"

show-ghc-version:
	@echo "GHC Version: $(GHC_VERSION)"

prepare-rust-interop:
	cd ./rust-interop/ && \
	cargo build --release && \
	mkdir -p ./.stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/ && \
	mkdir -p $(LINUX_HC_PATH) && \
	cp ./target/release/lib$(RUSTLIB).a $(LINUX_HC_PATH)/lib$(RUSTLIB).a && \
	cp ./target/release/lib$(RUSTLIB).a $(LINUX_HC_PATH)/lib$(RUSTLIB)_p.a && \
	cp ./target/release/lib$(RUSTLIB).so $(LINUX_HC_PATH)/lib$(RUSTLIB).so || true && \
	cp ./target/release/lib$(RUSTLIB).so $(LINUX_HC_PATH)/lib$(RUSTLIB)_p.so || true && \
	cp ./target/release/lib$(RUSTLIB).so $(LINUX_HC_PATH)/lib$(RUSTLIB)-ghc$(GHC_VERSION).so || true && \
	cp ./target/release/lib$(RUSTLIB).so $(LINUX_HC_PATH)/lib$(RUSTLIB)-ghc$(GHC_VERSION)_p.so || true && \
	cp ./target/release/lib$(RUSTLIB).a .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB).a || true && \
	cp ./target/release/lib$(RUSTLIB).a .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB)_p.a || true && \
	cp ./target/release/lib$(RUSTLIB).so .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB).so || true && \
	cp ./target/release/lib$(RUSTLIB).so .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB)_p.so || true && \
	cp ./target/release/lib$(RUSTLIB).so .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB)-ghc$(GHC_VERSION).so || true && \
	cp ./target/release/lib$(RUSTLIB).so .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB)-ghc$(GHC_VERSION)_p.so || true && \
	cp ./target/release/lib$(RUSTLIB).dylib .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB).dylib || true && \
	cp ./target/release/lib$(RUSTLIB).dylib .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB)_p.dylib || true && \
	cp ./target/release/lib$(RUSTLIB).dylib .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB)-ghc$(GHC_VERSION).dylib  || true
	cp ./target/release/lib$(RUSTLIB).dylib .stack-work/dist/$(OS_ARCH)/ghc-$(GHC_VERSION)/build/lib$(RUSTLIB)-ghc$(GHC_VERSION)_p.dylib  || true

.PHONY: all test fmt lint fix-lint lice-reload prepare-rust-interop

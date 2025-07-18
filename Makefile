# GHC_VERSION := $(shell stack ghc -- --version | awk '{print $$NF}')
# GHC_VERSION := $(shell stack ghc -- --version | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+' | head -n 1)
GHC_VERSION := 9.10.2
ARCH := $(shell uname -m | sed 's/arm64/aarch64/' | tr '[:upper:]' '[:lower:]')
OS := $(shell uname -s | sed 's/Darwin/osx/' | tr '[:upper:]' '[:lower:]')
OS_ARCH := $(ARCH)-$(OS)
LINUX_HC_PATH := ./stack-work/dist/x86_64-linux/ghc-$(GHC_VERSION)/apitoolkit-server-0.1.0.0/build
RUSTLIB := Crust_interop

css-start:
	./node_modules/.bin/tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --watch
post-css:
	./node_modules/.bin/tailwindcss --postcss ./static/public/assets/css/tailwind.css  -o ./static/public/assets/css/tailwind.min.css
run:
	cabal run

cypress:
	npx cypress run --record --key 2a2372e2-4ba1-4cd5-8bed-f39f4f047b3e

live-reload:
	ghcid --command 'cabal repl apitoolkit-server --ghc-options="-w -j4 -Wno-error=unused-imports -Wno-error=unused-top-binds" --with-compiler=ghc-9.10.2' --test ':run Start.startApp' --warnings

live-test-reload:
	ghcid --command 'cabal repl lib:apitoolkit-server test/unit/Main.hs --ghc-options="-w -j4" --with-compiler=ghc-9.10.2' --test ':run main' --warnings

live-test-reload-unit:
	ghcid --test 'cabal test apitoolkit-server:unit-tests --ghc-options="-w -j4" --test-show-details=streaming'

live-test-reload-all:
	ghcid --test 'cabal test apitoolkit-server:tests --ghc-options="-w -j4" --test-show-details=streaming'

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


live-test-reload-cabal:
	ghcid --test 'cabal test --ghc-options="-w -j4" --test-show-details=streaming'

test:
	USE_EXTERNAL_DB=true  cabal test -test-show-details=streaming --test-options='--color '

test-unit:
	cabal test unit-tests

test-doctests:
	cabal test doctests

test-integration:
	USE_EXTERNAL_DB=true cabal test integration-tests --test-show-details=streaming --test-options='--color '

live-test-unit:
	ghcid --test 'cabal test apitoolkit-server:unit-tests --ghc-options="-w -j4" --test-show-details=streaming'

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
	docker run -it --rm --name=apitoolkit -p 5432:5432/tcp \
		-e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=apitoolkit \
		--mount type=tmpfs,destination=/var/lib/postgresql/data,tmpfs-size=1G \
		docker.io/timescale/timescaledb-ha:pg16-all \
		-c shared_preload_libraries='pg_stat_statements,timescaledb' -c max_connections=200

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

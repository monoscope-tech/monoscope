# GHC_VERSION := $(shell stack ghc -- --version | awk '{print $$NF}')
# GHC_VERSION := $(shell stack ghc -- --version | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+' | head -n 1)
GHC_VERSION := 9.10.2
ARCH := $(shell uname -m | sed 's/arm64/aarch64/' | tr '[:upper:]' '[:lower:]')
OS := $(shell uname -s | sed 's/Darwin/osx/' | tr '[:upper:]' '[:lower:]')
OS_ARCH := $(ARCH)-$(OS)

css-start:
	./node_modules/.bin/tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --watch
post-css:
	./node_modules/.bin/tailwindcss --postcss ./static/public/assets/css/tailwind.css  -o ./static/public/assets/css/tailwind.min.css
run:
	cabal run

cypress:
	npx cypress run --record --key 2a2372e2-4ba1-4cd5-8bed-f39f4f047b3e

live-reload:
	ghcid --command 'cabal repl monoscope --ghc-options="-w -j4 -Wno-error=unused-imports -Wno-error=unused-top-binds" --with-compiler=ghc-9.10.2' --test ':run Start.startApp' --warnings

live-test-reload:
	ghcid --command 'cabal repl lib:monoscope test/unit/Main.hs --ghc-options="-w -j4" --with-compiler=ghc-9.10.2' --test ':run main' --warnings

live-test-reload-unit:
	ghcid --test 'cabal test monoscope:unit-tests --ghc-options="-w -j4" --test-show-details=streaming'

live-test-reload-all:
	ghcid --test 'cabal test monoscope:tests --ghc-options="-w -j4" --test-show-details=streaming'

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
	USE_EXTERNAL_DB=true  cabal test -j --ghc-options="-O0 -j8"  --test-show-details=direct --test-options='--color '

test-unit:
	cabal test unit-tests -j --ghc-options="-O0 -j8"  --test-show-details=direct --test-options='--color '

test-doctests:
	cabal test doctests -j --ghc-options="-O0 -j8" --test-show-details=direct 

test-integration:
	USE_EXTERNAL_DB=true cabal test integration-tests -j --ghc-options="-O0 -j8" --test-show-details=direct --test-options='--color '

live-test-unit:
	ghcid --test 'cabal test monoscope:unit-tests --ghc-options="-w -j4" --test-show-details=streaming'

live-reload-doctests:
	ghcid --command 'cabal repl lib:monoscope --ghc-options="-w -j4" --with-compiler=ghc-9.10.2' --test ':! cabal test monoscope:doctests --ghc-options="-O0 -j8" --test-show-details=streaming'

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
	docker run -it --rm --name=monoscope -p 5432:5432/tcp -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=monoscope -v $$HOME/pg-data:/home/postgres/pgdata \
		docker.io/timescale/timescaledb-ha:pg16-all -c shared_preload_libraries='pg_stat_statements,timescaledb'

timescaledb-docker-tmp:
	docker run -it --rm --name=monoscope -p 5432:5432/tcp \
		-e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=monoscope \
		--mount type=tmpfs,destination=/var/lib/postgresql/data,tmpfs-size=1G \
		docker.io/timescale/timescaledb-ha:pg16-all \
		-c shared_preload_libraries='pg_stat_statements,timescaledb' -c max_connections=200

update-service-worker:
	npx workbox generateSW workbox-config.js

show-os-arch:
	@echo "OS and Architecture: $(OS_ARCH)"

show-ghc-version:
	@echo "GHC Version: $(GHC_VERSION)"


.PHONY: all test fmt lint fix-lint live-reload live-reload-doctests

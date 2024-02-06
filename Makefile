css-start:
	npx tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --watch
post-css:
	npx postcss-cli ./static/public/assets/css/tailwind.css  -o ./static/public/assets/css/tailwind.min.css
run:
	stack run

cypress:
	npx cypress run --record --key 2a2372e2-4ba1-4cd5-8bed-f39f4f047b3e

live-reload:
	ghcid --command 'stack ghci apitoolkit-server --ghc-options=-w' --test ':run Start.startApp' --warnings

live-test-reload:
	ghcid --command 'stack ghci apitoolkit-server:apitoolkit-server-test --ghc-options=-w' --test ':run Main.main' --warnings

live-test-reload-stack:
	stack test --ghc-options=-w --file-watch
	# stack test --ghc-options=-w --ta='--match "SeedingConfig/should parse simple config to obj"' --file-watch

test:
	stack test --ghc-options=-w

fmt:
	fourmolu --mode inplace $$(find ./src/ -name '*.hs')

fix-imports:
	fix-imports $$(find ./src -name '*.hs') <$$(find ./src -name '*.hs')

lint:
	hlint src 

fix-lint:
	find ./src -name '*.hs' | xargs -L1 hlint --refactor --refactor-options="--inplace"

timescaledb-docker:
	docker run -it --rm --name=apitoolkit -p 5432:5432/tcp -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=apitoolkit -v $$HOME/pg-data:/home/postgres/pgdata \
		docker.io/timescale/timescaledb-ha:pg15-latest -c shared_preload_libraries='pg_stat_statements,timescaledb'

timescaledb-docker-tmp:
	docker run -it --rm --name=apitoolkit -p 5432:5432/tcp -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=apitoolkit \
		docker.io/timescale/timescaledb-ha:pg15-latest -c shared_preload_libraries='pg_stat_statements,timescaledb'

update-service-worker:
	workbox generateSW workbox-config.js

.PHONY: all test fmt lint fix-lint lice-reload 

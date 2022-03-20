css-start:
	npx tailwindcss -i ./static/public/assets/css/tailwind.css -o ./static/public/assets/css/tailwind.min.css --watch

run:
	stack run

live-reload:
	ghcid --command 'stack ghci apitoolkit-server --ghc-options=-w' --test ':run Main.main' --warnings

live-test-reload:
	ghcid --command 'stack ghci apitoolkit-server:apitoolkit-server-test --ghc-options=-w' --test ':run main' --warnings

live-test-reload-stack:
	stack test --ghc-options=-w --file-watch
	# stack test --ghc-options=-w --ta='--match "SeedingConfig/should parse simple config to obj"' --file-watch

fmt:
	ormolu --mode inplace $$(find . -name '*.hs')

lint:
	hlint src 

fix-lint:
	find ./src -name '*.hs' | xargs -L1 hlint --refactor --refactor-options="--inplace"

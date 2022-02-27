css-start:
	npx tailwindcss -i ./static/assets/css/tailwind.css -o ./static/assets/css/tailwind.min.css --watch

run:
	stack run

live-reload:
	ghcid --command 'stack ghci apitoolkit-server --ghc-options=-w' --test ':run Main.main' --warnings

fmt:
	ormolu --mode inplace $$(find . -name '*.hs')

lint:
	hlint src 

fix-lint:
	find ./src -name '*.hs' | xargs -L1 hlint --refactor --refactor-options="--inplace"

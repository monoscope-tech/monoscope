css-start:
	npx tailwindcss -i ./static/assets/css/tailwind.css -o ./static/assets/css/tailwind.min.css --watch

live-reload:
	ghcid --command 'stack ghci' --test ':run Main.main' --warnings

fmt:
	ormolu --mode inplace $$(find . -name '*.hs')

lint:
	hlint src 

fix-lint:
	find ./src -name '*.hs' | xargs -L1 hlint --refactor --refactor-options="--inplace"

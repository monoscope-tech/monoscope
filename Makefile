css-start:
	npx tailwindcss -i ./static/assets/css/tailwind.css -o ./static/assets/css/tailwind.min.css --watch

live-reload:
	ghcid --command 'stack ghci' --test ':run Main.main' --warnings

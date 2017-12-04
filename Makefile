.PHONY: all
all: ghcjs-build

.PHONY: ghc-build
ghc-build:
	nix-build -j 4 -A anapo-test-app --arg ghcjs false

.PHONY: ghcjs-build
ghcjs-configure:
	nix-shell -A anapo-test-app.env --run 'cd anapo-test-app && runhaskell Setup.hs configure --ghcjs'

.PHONY: ghcjs-build
ghcjs-build:
	nix-shell -A anapo-test-app.env --run 'cd anapo-test-app && runhaskell Setup.hs build'

.PHONY: serve-test-app
serve-test-app:
	cd anapo-test-app && python3 serve.py

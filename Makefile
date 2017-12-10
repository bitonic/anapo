.PHONY: all
all: ghcjs-test-app-build

.PHONY: ghc-anapo-configure
ghc-anapo-configure:
	nix-shell -A anapo.env --arg ghcjs false --run 'cd anapo && runhaskell Setup.hs configure'

.PHONY: ghc-test-app-configure
ghc-test-app-configure:
	nix-shell -A anapo-test-app.env --arg ghcjs false --run 'cd anapo-test-app && runhaskell Setup.hs configure'

.PHONY: ghc-anapo-build
ghc-anapo-build:
	nix-shell -A anapo.env --arg ghcjs false --run 'cd anapo && runhaskell Setup.hs build'

.PHONY: ghc-test-app-build
ghc-test-app-build:
	nix-shell -A anapo-test-app.env --arg ghcjs false --run 'cd anapo-test-app && runhaskell Setup.hs build'

.PHONY: ghc-anapo-repl
ghc-anapo-repl:
	nix-shell -A anapo.env --arg ghcjs false --run 'cd anapo && runhaskell Setup.hs repl'

.PHONY: ghc-test-app-repl
ghc-test-app-repl:
	nix-shell -A anapo-test-app.env --arg ghcjs false --run 'cd anapo-test-app && runhaskell Setup.hs repl'

.PHONY: ghcjs-anapo-configure
ghcjs-anapo-configure:
	nix-shell -A anapo.env --run 'cd anapo && runhaskell Setup.hs configure --ghcjs'

.PHONY: ghcjs-test-app-configure
ghcjs-test-app-configure:
	nix-shell -A anapo-test-app.env --run 'cd anapo-test-app && runhaskell Setup.hs configure --ghcjs'

.PHONY: ghcjs-anapo-build
ghcjs-anapo-build:
	nix-shell -A anapo.env --run 'cd anapo && runhaskell Setup.hs build'

.PHONY: ghcjs-test-app-build
ghcjs-test-app-build:
	nix-shell -A anapo-test-app.env --run 'cd anapo-test-app && runhaskell Setup.hs build'

.PHONY: ghcjs-anapo-repl
ghcjs-anapo-repl:
	nix-shell -A anapo.env --run 'cd anapo && runhaskell Setup.hs repl'

.PHONY: ghcjs-test-app-repl
ghcjs-test-app-repl:
	nix-shell -A anapo-test-app.env --run 'cd anapo-test-app && runhaskell Setup.hs repl'

.PHONY: serve-test-app
serve-test-app:
	cd anapo-test-app && python3 serve.py


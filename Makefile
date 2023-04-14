build:
	hpack; cabal new-build --allow-newer

repl:
	cabal new-repl --allow-newer

run: build

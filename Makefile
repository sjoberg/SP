TOP := $(dir $(lastword $(MAKEFILE_LIST)))

all: hlint install documentation

install:
	@echo "Building..."
	@cabal install --prefix="`readlink -f $(TOP)`/dist" --user -O2 --ghc-options="-threaded +RTS -N4 -RTS"

hlint:
	@echo "Running hlint..."
	@hlint src

documentation: 
	@cabal haddock

clean:
	@rm -rf dist

run:
	dist/bin/sp

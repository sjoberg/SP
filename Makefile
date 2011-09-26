REL_TOP := $(dir $(lastword $(MAKEFILE_LIST)))
TOP=`readlink -f $(REL_TOP)`/dist
PROF_FLAGS=-prof -fforce-recomp -auto-all
RTS_FLAGS=-rtsopts=all -threaded
all: build test

build:
	@echo "Building..."
	cabal install --prefix=$(TOP) --user -O2 --ghc-options="$(RTS_FLAGS)"

build-prof:
	@echo "Building..."
	@cabal install --prefix=$(TOP) --user -O2 --ghc-options="$(PROF_FLAGS) $(RTS_FLAGS)"

hlint:
	@echo "Running hlint..."
	@hlint src

doc: 
	@cabal haddock

clean:
	@rm -rf dist

test:
	@echo "Testing..."
	@runhaskell -w tests/AllTests.hs

run:
	dist/bin/sp +RTS -N -K100M -RTS
	@# +RTS -N -RTS # -K100M -RTS

run-prof:
	@dist/bin/sp +RTS -N1 -K100M -xc -RTS

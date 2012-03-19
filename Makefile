REL_TOP := $(dir $(lastword $(MAKEFILE_LIST)))
TOP=`readlink -f $(REL_TOP)`/dist
RTS_FLAGS=-rtsopts=all -threaded
GHC_FLAGS=-XTypeSynonymInstances -XBangPatterns 
#-O2 -fforce-recomp -funfolding-use-threshold=16 -fexcess-precision -fllvm
PROF_FLAGS=-prof -fforce-recomp -auto-all
all: build
build:
	@echo "Building..."
	@cabal install --prefix=$(TOP) --user -O2 --ghc-options="$(GHC_FLAGS) $(RTS_FLAGS)"

build-prof:
	@echo "Building..."
	@cabal install --prefix=$(TOP) --user -O2 --ghc-options="$(GHC_FLAGS) $(RTS_FLAGS) $(PROF_FLAGS)"

hlint:
	@echo "Running hlint..."
	@hlint src

doc: 
	@cabal haddock

clean:
	@rm -rf dist

test:
	@echo "Testing..."
	@runhaskell -w tests/Test1.hs

run:
	@dist/bin/sp +RTS -N -K100M -RTS -A4M -H1G

run-prof:
	@dist/bin/sp +RTS -N1 -K100M -xc -hc -L80 -RTS

deps:
	@cabal install --reinstall -O2 -p hashable unordered-containers deepseq regex-compat regex-posix regex-base MissingH hslogger network parsec compact-string-fix cryptohash crypto-api entropy tagged semigroups data-default dlist cereal data-binary-ieee754 monad-control base-unicode-symbols random-shuffle parallel bson lifted-base list-extras largeword ConfigFile mongoDB

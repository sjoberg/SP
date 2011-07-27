TOP := $(dir $(lastword $(MAKEFILE_LIST)))

all: install

install:
	@echo "Building..."
	@cabal install --prefix="`readlink -f $(TOP)`/dist" --user

clean:
	@@rm -rf dist/*


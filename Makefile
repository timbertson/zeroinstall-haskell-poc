.PHONY: phony
rainbow-build: phony
	rainbow --conf ghc -- cabal build

build: phony
	cabal build

configure: phony
	cabal configure

all: configure build

clean: phony
	find . -name '*.hi' -o -name '*.o' | parallel rm

i: phony
	cd src && ghci

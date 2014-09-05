


all: deps/GF-erc
	cabal sandbox init
	cabal sandbox add-source deps/GF-erc
	cabal install --only-dep
	cabal build

deps/GF-erc:
	mkdir -p deps
	git clone git@github.com:creswick/GF.git deps/GF-erc

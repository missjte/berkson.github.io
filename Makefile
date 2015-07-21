all:
	cabal clean
	cabal build
	./blog deploy

install:
	cabal clean
	cabal build
	./src/deploy.sh setup

nosync:
	cabal clean
	cabal build

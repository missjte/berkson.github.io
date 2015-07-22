all:
	cabal build
	./blog deploy

install:
	cabal build
	./src/deploy.sh setup

watch:
	cabal build
	./blog watch

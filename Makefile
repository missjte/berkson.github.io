all:
	cabal build
	./blog deploy

install:
	cabal build
	./src/deploy.sh setup

watch:
	cabal build
	./blog watch

clean:
	cabal clean
	cabal build
	./blog build

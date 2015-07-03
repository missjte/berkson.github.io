all:
	cabal build
	rm -r dist/
	./blog deploy

install:
	cabal build
	rm -r dist/
	./src/deploy.sh setup

nosync:
	cabal build
	rm -r dist/

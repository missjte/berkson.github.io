all:
	cabal build
	rm -r dist/
	./src/deploy.sh deploy

install:
	cabal build
	rm -r dist/
	./src/deploy.sh setup

nosync:
	cabal build
	rm -r dist/

clean:
	-cabal clean
	-rm -r generated/
	-rm -r deploy/

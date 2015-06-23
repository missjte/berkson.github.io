all:
	cabal build
	cp dist/build/blog/blog .
	./src/deploy.sh deploy

install:
	cabal build
	cp dist/build/blog/blog .
	./src/deploy.sh setup

nosync:
	cabal build
	cp dist/build/blog/blog .

clean:
	$(RM) blog
	find -type f -iregex ".+\.\(o\|hi\|hp\|mix\|ps\|tix\)" -exec rm -v {} \;

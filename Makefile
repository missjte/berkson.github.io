OBJ = dist/build/blog/blog-tmp/Main.o dist/build/base/base-tmp/Main.o
all: $(OBJ)
	cabal build
	cp dist/build/blog/blog .
	./deploy.sh deploy

install:
	cabal build
	cp dist/build/blog/blog .
	./deploy.sh setup

nosync: $(OBJ)
	cabal build
	cp dist/build/blog/blog .

clean:
	$(RM) blog
	find -type f -iregex ".+\.\(o\|hi\|hp\|mix\|ps\|tix\)" -exec rm -v {} \;

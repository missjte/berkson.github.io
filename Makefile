all:
	cabal build
	./blog deploy

install:
	cabal build
	./source/deploy.sh setup

watch:
	cabal build
	./blog build
	(cd generated/deploy/ && python3 -m http.server 4000)

clean:
	cabal clean
	cabal build
	./blog build

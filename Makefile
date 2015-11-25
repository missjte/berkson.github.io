all:
	stack build
	./blog deploy

install:
	stack build
	./source/deploy.sh setup

watch:
	stack build
	./blog watch

.PHONY: all build install test

all: build install test

build:
	dune build

install:
	dune install

test:
	dune test

.PHONY: all build install test

all: build install

build:
	dune build

install:
	dune install

test:
	dune test

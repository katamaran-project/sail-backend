.PHONY: all build install

all: build install

build:
	dune build

install:
	dune install

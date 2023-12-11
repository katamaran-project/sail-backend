.PHONY: all build install test

all: build install test

build:
	dune build

install:
	dune install

test:
	dune test

example: build install
	make -C working-example

minimal-caps: build install
	make -C minimal-caps

mc: minimal-caps

clean:
	rm -rf _build

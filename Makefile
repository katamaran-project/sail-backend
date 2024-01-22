.PHONY: all build install test

all: build install test minimal-caps example

full: clean all

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
	make -C working-example clean
	make -C minimal-caps clean

ws:
	ruby remove-trailing-whitespace.rb

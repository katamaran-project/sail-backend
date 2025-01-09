.PHONY: all build install test example clean ws ms minimal-caps

all: build install test coq-tests

full: clean all

build:
	dune build

install: build
	dune install

test:
	dune test

.PHONY: template-tests tt
template-tests: build install
	make -C tests/template-tests

.PHONY: coq-tests ct
coq-tests: build install
	make -C tests/coq-tests | tee test-results.txt


.PHONY: msp430-tests msp
msp430-tests: build install
	make -C tests/msp430-tests | tee test-results.txt


tt: template-tests

ct: coq-tests

msp: msp430-tests

example: build install
	make -C working-example

minimal-caps: build install
	make -C minimal-caps

mc: minimal-caps

clean:
	rm -rf _build

ws:
	ruby remove-trailing-whitespace.rb

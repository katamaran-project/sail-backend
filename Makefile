.PHONY: all
all: build install test coq-tests


.PHONY: full
full: clean all


.PHONY: build
build:
	dune build


.PHONY: install
install: build
	dune install


.PHONY: test
test:
	dune test


.PHONY: template-tests tt
template-tests: build install
	make -C tests/template-tests

tt: template-tests


.PHONY: coq-tests ct
coq-tests: build install
	make -C tests/coq-tests | tee test-results.txt

ct: coq-tests


.PHONY: msp430-tests msp
msp430-tests: build install
	make -C tests/msp430-tests | tee test-results.txt

msp: msp430-tests


.PHONY: example
example: build install
	make -C working-example


.PHONY: minimal-caps mc
minimal-caps: build install
	make -C minimal-caps

mc: minimal-caps


.PHONY: clean
clean:
	rm -rf _build


.PHONY: ws
ws:
	ruby remove-trailing-whitespace.rb

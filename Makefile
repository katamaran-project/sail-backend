.PHONY: all
all: build install test template-tests end-to-end-tests


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


.PHONY: end-to-end-tests e2e
end-to-end-tests: build install
	make -C tests/coq-tests | tee test-results.txt

e2e: end-to-end-tests


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

# The test targets use the plugin built in this checkout (via bin/sail-local)
# instead of installing it, so that several checkouts or worktrees can be
# tested in parallel without overwriting each other's installed plugin.
SAIL_LOCAL := $(CURDIR)/bin/sail-local


.PHONY: all
all: build test template-tests end-to-end-tests


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
template-tests: build
	SAIL=$(SAIL_LOCAL) make -C tests/template-tests

tt: template-tests


.PHONY: end-to-end-tests e2e
end-to-end-tests: build
	tests/coq-tests/run-tests.py

e2e: end-to-end-tests


.PHONY: msp430-tests msp
msp430-tests: build
	SAIL=$(SAIL_LOCAL) make -C tests/msp430-tests | tee test-results.txt

msp: msp430-tests


.PHONY: example
example: build
	make -C working-example SAIL=$(SAIL_LOCAL)


.PHONY: minimal-caps mc
minimal-caps: build
	make -C minimal-caps SAIL=$(SAIL_LOCAL)

mc: minimal-caps


.PHONY: clean
clean:
	rm -rf _build


.PHONY: ws
ws:
	remove-trailing-whitespace

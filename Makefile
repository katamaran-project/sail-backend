# The test targets use the plugin built in this checkout (via bin/sail-local)
# instead of installing it, so that several checkouts or worktrees can be
# tested in parallel without overwriting each other's installed plugin.
SAIL_LOCAL := $(CURDIR)/bin/sail-local


.PHONY: all
all: build test cheriot-lib-tests template-tests end-to-end-tests


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


.PHONY: cheriot-lib-tests
cheriot-lib-tests:
	python3 -m unittest discover -s tests/cheriot/tests -t tests/cheriot


# The CHERIoT progress loop (case-studies/cheriot/docs/loop.md). Needs a cheriot-sail
# checkout; the case study is case-studies/cheriot (CHERIOT_CASE_STUDY overrides it), e.g.
#   make cheriot CHERIOT_SAIL_DIR=/path/to/cheriot-sail
# CHERIOT_ARGS passes extra options to tests/cheriot/run.py, e.g. CHERIOT_ARGS=--keep-dumps.
.PHONY: cheriot
cheriot: build
	tests/cheriot/run.py $(CHERIOT_ARGS)


# Translate the CHERIoT core to Rocq (base.v, machine.v) in CHERIOT_OUTPUT, replacing what is there, e.g.
#   make cheriot-translate CHERIOT_SAIL_DIR=/path/to/cheriot-sail
# CHERIOT_ARGS=--no-scope translates the whole model.
CHERIOT_OUTPUT := case-studies/cheriot/output
.PHONY: cheriot-translate
cheriot-translate: build
	rm -rf $(CHERIOT_OUTPUT)
	tests/cheriot/run.py --translate-to $(CHERIOT_OUTPUT) $(CHERIOT_ARGS)


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


.PHONY: minimal-caps mc
minimal-caps: build
	make -C case-studies/minimal-caps SAIL=$(SAIL_LOCAL)

mc: minimal-caps


# Translation validation (validation/docs/validate.md): the programs in validation/cases run in
# µSail (Rocq) and Sail's C emulator, states compared after every step. Builds what is missing
# (emulator ~10 min, model ~2 min); Rocq results cached by input hash. Under the sail-backend switch:
#   opam exec --switch=sail-backend -- gmake validate [VALIDATE_CHERIOT=DIR] [VALIDATE_ARGS="addi -j 8"]
# VALIDATE_CHERIOT: the cheriot-sail clone (default: $CHERIOT_SAIL_DIR, else ../cheriot-sail).
VALIDATE_CHERIOT ?= $(or $(CHERIOT_SAIL_DIR),$(abspath $(CURDIR)/../cheriot-sail))

.PHONY: validate
validate: build
	$(MAKE) -C validation/emulator CHERIOT=$(VALIDATE_CHERIOT)
	test -f validation/build/model/base.v || $(MAKE) -C validation/rocq model CHERIOT_SAIL_DIR=$(VALIDATE_CHERIOT)
	$(MAKE) -C validation/rocq validate
	validation/scripts/validate.py $(VALIDATE_ARGS)


.PHONY: clean
clean:
	rm -rf _build


.PHONY: ws
ws:
	./remove-trailing-whitespaces

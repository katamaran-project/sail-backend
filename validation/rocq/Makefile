# Build: opam exec --switch=sail-backend -- gmake -C validation/rocq
COQC  ?= coqc
FLAGS := -Q . Validation

all: Interp.vo InterpTests.vo

%.vo: %.v
	$(COQC) $(FLAGS) $<

InterpTests.vo: Interp.vo

clean:
	rm -f *.vo *.vok *.vos *.glob .*.aux

.PHONY: all clean

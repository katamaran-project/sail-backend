# Build: opam exec --switch=sail-backend -- gmake -C validation/rocq [all|cheriot|model]
#   all      Interp.vo, InterpTests.vo
#   cheriot  the CHERIoT harness, StepADDI and the ADDI dumps (compiles the model in ../build/model if needed)
#   dumps    the ADDI state dumps in ../build/dumps (DumpADDI.v; docs/state-format.md)
#   model    regenerate base.v/machine.v with this checkout's plugin (needs CHERIOT_SAIL_DIR)
COQC  ?= coqc
ROOT  := $(abspath ../..)
BUILD := $(abspath ../build)
MODEL := $(BUILD)/model
RUN   := $(BUILD)/cheriot-run
DUMPS := $(BUILD)/dumps
FLAGS := -Q . Validation -R $(MODEL) CheriotModel
HISTORY := $(ROOT)/case-studies/cheriot/docs/history.csv
STATEFILE := python3 $(abspath ../compare/statefile.py)

all: Interp.vo InterpTests.vo

cheriot: StepADDI.vo dumps

%.vo: %.v
	$(COQC) $(FLAGS) $<

InterpTests.vo: Interp.vo
Harness.vo: Interp.vo $(MODEL)/machine.vo
StepADDI.vo: Harness.vo
Dump.vo: Harness.vo Names.vo

# Compiling DumpADDI.v writes the dumps (Redirect writes <name>.out; renamed to <name>).
# Then: grammar check, and Rocq's delta must equal statefile.py's delta of the two states.
dumps: DumpADDI.vo

DumpADDI.vo: DumpADDI.v Dump.vo StepADDI.vo ../compare/statefile.py
	mkdir -p $(DUMPS)/addi
	$(COQC) $(FLAGS) -output-directory $(DUMPS) $<
	for f in $(DUMPS)/*.out $(DUMPS)/addi/*.out; do mv -f "$$f" "$${f%.out}"; done
	$(STATEFILE) check $(DUMPS)/addi/reset.state $(DUMPS)/addi/step1.state $(DUMPS)/addi/step1.delta
	$(STATEFILE) check --run $(DUMPS)/addi/run.states
	$(STATEFILE) delta $(DUMPS)/addi/reset.state $(DUMPS)/addi/step1.state \
	    | diff -u $(DUMPS)/addi/step1.delta -

# The model: tests/cheriot/run.py (scope.toml, configuration, templates), without Rocq.
# run.py appends to the tracked history.csv: restored afterwards.
# The .v files are copied only when they changed, so machine.v (1 min) recompiles only then.
model:
	@test -n "$(CHERIOT_SAIL_DIR)" || { echo "set CHERIOT_SAIL_DIR" >&2; exit 2; }
	cd $(ROOT) && dune build
	mkdir -p $(MODEL)
	cp $(HISTORY) $(BUILD)/history.csv.saved
	cd $(ROOT) && tests/cheriot/run.py --cheriot-sail $(CHERIOT_SAIL_DIR) --no-coq \
	    --output-dir $(RUN) --keep-runs 1; status=$$?; \
	    cp $(BUILD)/history.csv.saved $(HISTORY); exit $$status
	for f in base.v machine.v; do \
	    cmp -s $(RUN)/latest/$$f $(MODEL)/$$f || cp $(RUN)/latest/$$f $(MODEL)/$$f; done

$(MODEL)/base.v $(MODEL)/machine.v:
	@echo "$@ missing: run 'gmake model CHERIOT_SAIL_DIR=...' first" >&2; exit 2

$(MODEL)/base.vo: $(MODEL)/base.v
	cd $(MODEL) && $(COQC) -R . CheriotModel base.v

$(MODEL)/machine.vo: $(MODEL)/machine.v $(MODEL)/base.vo
	cd $(MODEL) && $(COQC) -R . CheriotModel machine.v

clean:
	rm -f *.vo *.vok *.vos *.glob .*.aux

clean-model:
	rm -rf $(BUILD)

.PHONY: all cheriot dumps model clean clean-model

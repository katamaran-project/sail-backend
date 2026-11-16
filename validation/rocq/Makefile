# Build: opam exec --switch=sail-backend -- gmake -C validation/rocq [all|cheriot|model]
#   all      Interp.vo, InterpTests.vo
#   cheriot  the CHERIoT harness and StepADDI (compiles the model in ../build/model if needed)
#   model    regenerate base.v/machine.v with this checkout's plugin (needs CHERIOT_SAIL_DIR)
COQC  ?= coqc
ROOT  := $(abspath ../..)
BUILD := $(abspath ../build)
MODEL := $(BUILD)/model
RUN   := $(BUILD)/cheriot-run
FLAGS := -Q . Validation -R $(MODEL) CheriotModel
HISTORY := $(ROOT)/case-studies/cheriot/docs/history.csv

all: Interp.vo InterpTests.vo

cheriot: StepADDI.vo

%.vo: %.v
	$(COQC) $(FLAGS) $<

InterpTests.vo: Interp.vo
Harness.vo: Interp.vo $(MODEL)/machine.vo
StepADDI.vo: Harness.vo

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

.PHONY: all cheriot model clean clean-model

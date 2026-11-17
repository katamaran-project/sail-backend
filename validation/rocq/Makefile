# Build: opam exec --switch=sail-backend -- gmake -C validation/rocq [all|cheriot|model]
#   all      Interp.vo, InterpTests.vo
#   cheriot  the CHERIoT harness, StepADDI and the ADDI dumps (compiles the model in ../build/model if needed)
#   dumps    the ADDI state dumps in ../build/dumps (DumpADDI.v; docs/state-format.md)
#   validate Validate.vos, the runner of gmake validate (docs/validate.md); the model as .vos
#   model    regenerate base.v/machine.v with this checkout's plugin (needs CHERIOT_SAIL_DIR)
COQC  ?= coqc
ROOT  := $(abspath ../..)
BUILD := $(abspath ../build)
MODEL := $(BUILD)/model
RUN   := $(BUILD)/cheriot-run
DUMPS := $(BUILD)/dumps
FLAGS := -Q . Validation -R $(MODEL) CheriotModel
STATEFILE := python3 $(abspath ../compare/statefile.py)

all: Interp.vo InterpTests.vo

cheriot: StepADDI.vo dumps

%.vo: %.v
	$(COQC) $(FLAGS) $<

# gmake validate uses .vos files (coqc -vos): definitions are checked, Qed
# proofs skipped (gmake cheriot checks them).  The interpreter never unfolds a
# Qed proof, so the runs are the same; machine.v compiles in half the time.
%.vos: %.v
	$(COQC) -vos $(FLAGS) $<

InterpTests.vo: Interp.vo
Harness.vo: Interp.vo $(MODEL)/machine.vo
StepADDI.vo: Harness.vo
Dump.vo: Harness.vo Names.vo
Validate.vo: Dump.vo

Harness.vos: Interp.vos $(MODEL)/machine.vos
Dump.vos: Harness.vos Names.vos
Validate.vos: Dump.vos

validate: Validate.vos

# Compiling DumpADDI.v writes the dumps (Redirect writes <name>.out; renamed to <name>).
# Then: grammar check, Rocq's delta must equal statefile.py's delta of the two states,
# and Rocq's delta run file, rebuilt by statefile.py, must equal the full run file.
dumps: DumpADDI.vo

DumpADDI.vo: DumpADDI.v Dump.vo StepADDI.vo ../compare/statefile.py
	mkdir -p $(DUMPS)/addi $(DUMPS)/addi-add-sw
	$(COQC) $(FLAGS) -output-directory $(DUMPS) $<
	for f in $(DUMPS)/*.out $(DUMPS)/*/*.out; do mv -f "$$f" "$${f%.out}"; done
	$(STATEFILE) check $(DUMPS)/addi/reset.state $(DUMPS)/addi/step1.state $(DUMPS)/addi/step1.delta
	$(STATEFILE) check --run $(DUMPS)/addi/run.states $(DUMPS)/addi-add-sw/run.states
	$(STATEFILE) delta $(DUMPS)/addi/reset.state $(DUMPS)/addi/step1.state \
	    | diff -u $(DUMPS)/addi/step1.delta -
	$(STATEFILE) undelta $(DUMPS)/addi-add-sw/run.delta | diff -u $(DUMPS)/addi-add-sw/run.states -

# The model: tests/cheriot/run.py --translate-to (scope.toml, configuration,
# templates; no typecheck, analysis or history line).
# The .v files are copied only when they changed, so the model recompiles only then.
model:
	@test -n "$(CHERIOT_SAIL_DIR)" || { echo "set CHERIOT_SAIL_DIR" >&2; exit 2; }
	cd $(ROOT) && dune build
	mkdir -p $(MODEL)
	rm -rf $(RUN)
	cd $(ROOT) && tests/cheriot/run.py \
	    --cheriot-sail $(CHERIOT_SAIL_DIR) --translate-to $(RUN)
	for f in base.v machine.v; do \
	    cmp -s $(RUN)/$$f $(MODEL)/$$f || cp $(RUN)/$$f $(MODEL)/$$f; done

$(MODEL)/base.v $(MODEL)/machine.v:
	@echo "$@ missing: run 'gmake model CHERIOT_SAIL_DIR=...' first" >&2; exit 2

$(MODEL)/base.vo: $(MODEL)/base.v
	cd $(MODEL) && $(COQC) -R . CheriotModel base.v

$(MODEL)/machine.vo: $(MODEL)/machine.v $(MODEL)/base.vo
	cd $(MODEL) && $(COQC) -R . CheriotModel machine.v

$(MODEL)/base.vos: $(MODEL)/base.v
	cd $(MODEL) && $(COQC) -vos -R . CheriotModel base.v

$(MODEL)/machine.vos: $(MODEL)/machine.v $(MODEL)/base.vos
	cd $(MODEL) && $(COQC) -vos -R . CheriotModel machine.v

clean:
	rm -f *.vo *.vok *.vos *.glob .*.aux

clean-model:
	rm -rf $(BUILD)

.PHONY: all cheriot dumps validate model clean clean-model

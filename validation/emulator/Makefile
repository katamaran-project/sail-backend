# CHERIoT C emulator with a per-step state dump (validation/docs/validate.md).
#
#   gmake               build $(BUILD)/cheriot_sim_dump (~1 min cold: the model's C at -O0)
#   gmake dump-sail     regenerate dump_state.sail from the Rocq dump's register set
#   gmake check-regs    run addi-add-sw, compare the key set of step 0 with the Rocq reset state
#   gmake check         check-regs, then diff with the Rocq demos (ADDI step 1, addi-add-sw run)
#
# Run under the sail-backend switch: opam exec --switch=sail-backend -- gmake -C validation/emulator
# Nothing in the cheriot-sail clone is edited; its Makefile is only queried (query.mk).

EMU      := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
VAL      := $(abspath $(EMU)/..)
CHERIOT  ?= $(abspath $(VAL)/../../cheriot-sail)
BUILD    ?= $(VAL)/build/emulator
ROCQ_RESET ?= $(VAL)/build/dumps/addi/reset.state
# Sail's cache of SMT results (a cold one costs about half a minute); kept by `gmake clean`.
SAIL_SMT_CACHE ?= $(VAL)/build/sail_smt_cache

# The model's C: -O1 takes about 10 minutes, cheriot-sail's -O2 -flto over 30; a run takes
# milliseconds at -O0. The other C files are small.
MODEL_C_FLAGS ?= -O0
C_OPT_FLAGS   ?= -O1 -g

# The model's calls to these go through dump_state.c, which logs the written addresses.
RENAME = -Dplatform_write_mem=dump_platform_write_mem -Dwrite_tag_bool=dump_write_tag_bool

MODEL    = $(BUILD)/riscv_model_RV32
OBJ      = $(BUILD)/obj
SIM      = $(BUILD)/cheriot_sim_dump
# As validate.py (EMU_ARGS): the platform settings of tests/cheriot/scope.toml.
SIM_ARGS = -F -W --insns-per-tick 1000000000

all: $(SIM)
.PHONY: all dump-sail check-regs check clean
.DELETE_ON_ERROR:

# cheriot-sail's variables; C_OPT_FLAGS are ours. Rewritten whenever its inputs are newer:
# nothing depends on its date, only on the commands below.
$(BUILD)/vars.mk: $(EMU)/query.mk $(CHERIOT)/Makefile
	mkdir -p $(BUILD)
	$(MAKE) -s --no-print-directory -C $(CHERIOT) -f Makefile -f $(EMU)/query.mk print-vars \
	  C_OPT_FLAGS= > $@.tmp
	mv $@.tmp $@

include $(BUILD)/vars.mk

MODEL_SRCS   = $(addprefix $(CHERIOT)/,$(CS_SAIL_SRCS) $(CS_SAIL_RISCV_MODEL_DIR)/main.sail)
SUPPORT_SRCS = $(BUILD)/riscv_sim.c $(EMU)/dump_state.c $(CS_C_SRCS) $(wildcard $(CS_SAIL_LIB_DIR)/*.c)
SUPPORT_OBJS = $(patsubst %.c,$(OBJ)/%.o,$(notdir $(SUPPORT_SRCS)))

SAIL_CMD = $(CS_SAIL) $(CS_SAIL_FLAGS) $(CS_c_preserve_fns) -c_preserve dump_state \
  -O -Oconstant_fold --memo-z3-path $(SAIL_SMT_CACHE) --no-warn -c \
  -c_include riscv_prelude.h -c_include riscv_platform.h -c_include dump_state.h -c_no_main \
  $(CS_SAIL_SRCS) $(EMU)/dump_state.sail $(CS_SAIL_RISCV_MODEL_DIR)/main.sail
MODEL_CC_CMD = $(CC) -c $(CS_C_FLAGS) $(MODEL_C_FLAGS) -DNO_BOOT_ROM=1 -I $(EMU) $(RENAME) -MMD -MP
CC_CMD       = $(CC) -c $(CS_C_FLAGS) $(C_OPT_FLAGS) -DNO_BOOT_ROM=1 -I $(EMU) -MMD -MP
LINK_CMD     = $(CC) $(MODEL).o $(SUPPORT_OBJS) $(CS_C_LIBS) -o $(SIM)

# Command signatures: $(BUILD)/NAME.cmd holds a command, rewritten when the command changes.
# Targets depend on these instead of on the Makefiles: a flag change rebuilds what it affects,
# a comment edit or a touched Makefile rebuilds nothing.
differ   = $(subst $1,,$2)$(subst $2,,$1)
keep-cmd = $(if $(call differ,$(file <$(BUILD)/$1.cmd),$2),$(file >$(BUILD)/$1.cmd,$2))
ifdef CS_SAIL
$(call keep-cmd,sail,$(SAIL_CMD))
$(call keep-cmd,model-cc,$(MODEL_CC_CMD))
$(call keep-cmd,cc,$(CC_CMD))
$(call keep-cmd,link,$(LINK_CMD))
endif

# Sail's output replaces the previous one only if it differs: an edit that leaves the C
# unchanged (a touched or commented Sail file) costs the Sail run (seconds), not the C compile.
$(MODEL).c $(MODEL).h: $(BUILD)/model.stamp ;
$(BUILD)/model.stamp: $(MODEL_SRCS) $(EMU)/dump_state.sail $(BUILD)/sail.cmd
	mkdir -p $(BUILD)/sail-out $(dir $(SAIL_SMT_CACHE))
	cd $(CHERIOT) && $(SAIL_CMD) -o $(BUILD)/sail-out/riscv_model_RV32
	for f in c h; do cmp -s $(BUILD)/sail-out/riscv_model_RV32.$$f $(MODEL).$$f || \
	  cp $(BUILD)/sail-out/riscv_model_RV32.$$f $(MODEL).$$f; done
	touch $@

$(MODEL).o: $(MODEL).c $(BUILD)/model-cc.cmd
	$(MODEL_CC_CMD) $< -o $@

$(BUILD)/riscv_sim.c: $(CHERIOT)/sail-riscv/c_emulator/riscv_sim.c $(EMU)/riscv_sim.patch
	patch -s -o $@ $< $(EMU)/riscv_sim.patch

define support-obj
$(OBJ)/$(basename $(notdir $1)).o: $1 $(BUILD)/cc.cmd
	@mkdir -p $(OBJ)
	$$(CC_CMD) $$< -o $$@
endef
$(foreach s,$(SUPPORT_SRCS),$(eval $(call support-obj,$s)))

$(SIM): $(MODEL).o $(SUPPORT_OBJS) $(BUILD)/link.cmd
	$(LINK_CMD)

-include $(wildcard $(BUILD)/*.d $(OBJ)/*.d)

dump-sail: $(ROCQ_RESET) $(BUILD)/vars.mk
	cd $(CHERIOT) && $(EMU)/gen-dump.py $(ROCQ_RESET) $(CS_SAIL_SRCS) > $(EMU)/dump_state.sail.tmp
	cmp -s $(EMU)/dump_state.sail.tmp $(EMU)/dump_state.sail && rm $(EMU)/dump_state.sail.tmp || \
	  mv $(EMU)/dump_state.sail.tmp $(EMU)/dump_state.sail

# Runs of validation/scripts/examples/*.s: run file and per-step states.
RUNS = $(BUILD)/runs
$(RUNS)/%.states: $(VAL)/scripts/examples/%.s $(SIM)
	mkdir -p $(RUNS)
	$(VAL)/scripts/mk-test-elf.py $< $(RUNS)/$*.elf > /dev/null
	$(SIM) $(SIM_ARGS) --dump-state $@.tmp $(RUNS)/$*.elf > $(RUNS)/$*.log
	$(VAL)/compare/statefile.py check --run $@.tmp
	mv $@.tmp $@
	$(VAL)/compare/statefile.py split $@ $(RUNS)/$*

# Register keys after reset = the Rocq side's.
check-regs: $(RUNS)/addi-add-sw.states $(ROCQ_RESET)
	$(VAL)/compare/statefile.py keys $(ROCQ_RESET) $(RUNS)/addi-add-sw/reset.state

# The Rocq demos (gmake -C validation/rocq dumps): ADDI step 1 and the addi-add-sw run.
check: check-regs $(RUNS)/addi-x1.states
	diff -u $(VAL)/build/dumps/addi/step1.state $(RUNS)/addi-x1/step1.state
	diff -u $(VAL)/build/dumps/addi-add-sw/run.states $(RUNS)/addi-add-sw.states

clean:
	rm -rf $(BUILD)

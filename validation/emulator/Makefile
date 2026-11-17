# CHERIoT C emulator with a per-step state dump (validation/docs/validate.md).
#
#   gmake               build $(BUILD)/cheriot_sim_dump (~10 min: the model's C at -O1)
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

C_OPT_FLAGS ?= -O1
# Without Zb*/Zbk* (validate.md, "Emulator").
NOZB_INST = sail-riscv/model/riscv_insts_base.sail sail-riscv/model/riscv_insts_cext.sail \
  sail-riscv/model/riscv_insts_mext.sail sail-riscv/model/riscv_insts_zicsr.sail \
  sail-riscv/model/riscv_insts_hints.sail src/cheri_insts_begin.sail src/cheri_insts.sail \
  src/cheri_insts_cext.sail src/cheri_insts_end.sail

# The model's calls to these go through dump_state.c, which logs the written addresses.
RENAME = -Dplatform_write_mem=dump_platform_write_mem -Dwrite_tag_bool=dump_write_tag_bool

SIM      = $(BUILD)/cheriot_sim_dump
SIM_ARGS = --disable-compressed -F -W --insns-per-tick 1000000000

all: $(SIM)
.PHONY: all dump-sail check-regs check clean

$(BUILD)/vars.mk: $(EMU)/query.mk $(CHERIOT)/Makefile
	mkdir -p $(BUILD)
	$(MAKE) -s --no-print-directory -C $(CHERIOT) -f Makefile -f $(EMU)/query.mk print-vars \
	  C_OPT_FLAGS="$(C_OPT_FLAGS)" SAIL_DEFAULT_INST="$(NOZB_INST)" > $@.tmp
	mv $@.tmp $@

include $(BUILD)/vars.mk

MODEL_SRCS = $(addprefix $(CHERIOT)/,$(CS_SAIL_SRCS) $(CS_SAIL_RISCV_MODEL_DIR)/main.sail)

$(BUILD)/riscv_model_RV32.c: $(MODEL_SRCS) $(EMU)/dump_state.sail $(BUILD)/vars.mk
	cd $(CHERIOT) && $(CS_SAIL) $(CS_SAIL_FLAGS) $(CS_c_preserve_fns) -c_preserve dump_state \
	  -O -Oconstant_fold -memo_z3 -c -c_include riscv_prelude.h -c_include riscv_platform.h \
	  -c_include dump_state.h -c_no_main $(CS_SAIL_SRCS) $(EMU)/dump_state.sail \
	  $(CS_SAIL_RISCV_MODEL_DIR)/main.sail -o $(basename $@)

$(BUILD)/riscv_model_RV32.o: $(BUILD)/riscv_model_RV32.c $(EMU)/dump_state.h
	cd $(CHERIOT) && $(CC) -c -g $(CS_C_FLAGS) -DNO_BOOT_ROM=1 -I $(EMU) $(RENAME) $< -o $@

$(BUILD)/riscv_sim.c: $(CHERIOT)/sail-riscv/c_emulator/riscv_sim.c $(EMU)/riscv_sim.patch
	patch -s -o $@ $< $(EMU)/riscv_sim.patch

$(SIM): $(BUILD)/riscv_model_RV32.o $(BUILD)/riscv_sim.c $(EMU)/dump_state.c $(EMU)/dump_state.h $(EMU)/dump_sim.h
	cd $(CHERIOT) && $(CC) -g $(CS_C_FLAGS) -DNO_BOOT_ROM=1 -I $(EMU) $(BUILD)/riscv_model_RV32.o \
	  $(BUILD)/riscv_sim.c $(EMU)/dump_state.c $(CS_C_SRCS) $(CS_SAIL_LIB_DIR)/*.c $(CS_C_LIBS) -o $@

dump-sail: $(ROCQ_RESET) $(BUILD)/vars.mk
	cd $(CHERIOT) && $(EMU)/gen-dump.py $(ROCQ_RESET) $(CS_SAIL_SRCS) > $(EMU)/dump_state.sail.tmp
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

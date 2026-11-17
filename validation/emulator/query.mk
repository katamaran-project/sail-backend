# Loaded after cheriot-sail's Makefile (-f Makefile -f query.mk): prints its build variables.
# Paths into sail-riscv in the C variables are made absolute, so the C compiles run from any
# directory and their dependency files (-MMD) name existing files.
QUERY_VARS   = SAIL SAIL_FLAGS SAIL_SRCS SAIL_RISCV_MODEL_DIR SAIL_LIB_DIR c_preserve_fns
QUERY_C_VARS = C_FLAGS C_SRCS C_LIBS
abs = $(patsubst $(SAIL_RISCV_DIR)/%,$(CURDIR)/$(SAIL_RISCV_DIR)/%,$1)
print-vars:
	@:$(foreach v,$(QUERY_VARS),$(info CS_$(v) := $($(v))))
	@:$(foreach v,$(QUERY_C_VARS),$(info CS_$(v) := $(call abs,$($(v)))))

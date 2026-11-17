# Loaded after cheriot-sail's Makefile (-f Makefile -f query.mk): prints its build variables.
QUERY_VARS = SAIL SAIL_FLAGS SAIL_SRCS SAIL_RISCV_MODEL_DIR SAIL_LIB_DIR c_preserve_fns C_FLAGS C_SRCS C_INCS C_LIBS
print-vars:
	@:$(foreach v,$(QUERY_VARS),$(info CS_$(v) := $($(v))))

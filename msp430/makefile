.DEFAULT_GOAL := all
.PHONY: all cleanIntermediate testInt testUnit
.SECONDARY:

CD = _compilation
compiledSail = msp430
compiler = gcc


SAIL_DIR ?= $(shell opam var sail:share)
SAIL_LIB_DIR = $(SAIL_DIR)/lib

GMP_FLAGS  = $(shell pkg-config --cflags gmp)
GMP_LIBS   = $(shell pkg-config --libs gmp)
ZLIB_FLAGS = $(shell pkg-config --cflags zlib)
ZLIB_LIBS  = $(shell pkg-config --libs zlib)

C_FLAGS = -I $(SAIL_LIB_DIR) $(GMP_FLAGS) $(ZLIB_FLAGS)
C_LIBS  = $(GMP_LIBS) $(ZLIB_LIBS)

#defining the instructions
MSP430_BASE_INST =  instructions/doubleops.sail
MSP430_BASE_INST += instructions/singleops.sail 
MSP430_BASE_INST += instructions/jumps.sail
MSP430_EXTENDED_INST = $(MSP430_BASE_INST) # add extra files here fore extentions
MSP430_INST  = instructions/instructionsBegin.sail $(MSP430_EXTENDED_INST) instructions/instructionsEnd.sail

#defining everything else # TODO: this can be split up
MSP430_BASE = prelude.sail
MSP430_BASE += MSP430_exceptions.sail

MSP430_BASE += MSP430_types.sail
MSP430_BASE += log.sail

MSP430_BASE += regfile.sail
MSP430_BASE += wordBytePrimitives.sail
MSP430_BASE += statusBits.sail
MSP430_BASE += addressSpace.sail
MSP430_BASE += regsV3.sail


SAIL_SRCS = $(MSP430_BASE) $(MSP430_INST)

all: msp430

testInt: 
	$(MAKE) -C tests/integration

testUnit: msp430test
	./msp430test

%: $(CD)/%
	cp $< $@

$(CD)/%: $(CD)/%.c
	gcc $< $(SAIL_LIB_DIR)/*.c $(C_FLAGS) $(C_LIBS) -o $@

$(CD)/%.c: %.sail $(CD)/.f $(SAIL_SRCS)
	sail -c $(SAIL_SRCS) $< -o $(basename $@)

$(CD)/%.c: tests/unit/%.sail $(CD)/.f $(SAIL_SRCS) tests/unit/*.sail
	sail -c $(SAIL_SRCS) $< -o $(basename $@)

$(CD)/.f:
	mkdir -p $(dir $@)
	touch $@

cleanIntermediate:
	rm -rf $(CD)
.DEFAULT_GOAL := all
.PHONY: all clean
.SECONDARY:

#defining the instructions
MSP430_BASE_INST =  model/instructions/doubleops.sail
MSP430_BASE_INST += model/instructions/singleops.sail
MSP430_BASE_INST += model/instructions/jumps.sail
MSP430_EXTENDED_INST = $(MSP430_BASE_INST) # add extra files here fore extentions
MSP430_INST  = model/instructions/instructionsBegin.sail $(MSP430_EXTENDED_INST) model/instructions/instructionsEnd.sail

#defining everything else # TODO: this can be split up
MSP430_BASE = model/prelude.sail
MSP430_BASE += model/MSP430_exceptions.sail

MSP430_BASE += model/MSP430_types.sail
MSP430_BASE += model/log.sail

MSP430_BASE += model/regfile.sail
MSP430_BASE += model/wordBytePrimitives.sail
MSP430_BASE += model/statusBits.sail
MSP430_BASE += model/mpu.sail
MSP430_BASE += model/addressSpace.sail
MSP430_BASE += model/regsV3.sail

SAIL_SRCS = $(MSP430_BASE) $(MSP430_INST)

%.sail: %.sail.pp
	cpp -P $< -o $@

%.vo: %.v
	coqc $<

base.v machine.v: $(SAIL_SRCS)
	sail --katamaran --katamaran-config configuration.lisp $(SAIL_SRCS)

all: base.vo machine.vo

clean:
	rm -rf base.v machine.v *.vo *.vok *.vos *.glob .*.aux

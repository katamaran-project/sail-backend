# ADD.UW (Zba, RV64 only: OP-32) on RV32: illegal instruction on both sides, MTCC handler.
addi a1, zero, 5
.word 0x08c5853b
addi a2, zero, 99         # not reached

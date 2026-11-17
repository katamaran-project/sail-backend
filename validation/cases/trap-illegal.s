# Illegal instruction (encoding 0): trap to the handler (mcause 2).
addi a1, zero, 5
.word 0x00000000
addi a2, zero, 99         # not reached

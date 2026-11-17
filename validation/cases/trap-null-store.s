# Store through the null capability: CHERI tag violation (mcause 0x1c,
# mtval 0x142 = reg 10, tag), trap to MTCC = the epilogue's handler.
addi a1, zero, 5
sw a1, 0(ca0)             # ca0 is null at reset
addi a2, zero, 99         # not reached

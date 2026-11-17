# RV32E: LW into x16: the load reads memory, then `wX` throws
# `Error_not_rv32e_register`: traps illegal (mcause 2), x16 not written.
cspecialr ca0, mtdc       # root memory capability
li a1, 0x80002000
csetaddr ca0, ca0, a1
li a2, 0x12345678
sw a2, 0(ca0)
lw x16, 0(ca0)
addi a3, zero, 99         # not reached

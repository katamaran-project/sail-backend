# Load from an unmapped CLINT offset: load access fault (mcause 5).
cspecialr ca0, mtdc
li a1, 0x2000008
csetaddr ca0, ca0, a1
lw a2, 0(ca0)
addi a3, zero, 99            # not reached

# LW through a sealed data capability (otype 9): seal violation (mcause 0x1c).
cspecialr ca0, mtdc
li t0, 0x80002000
csetaddr ca0, ca0, t0
cspecialr cs0, mscratchc
addi t0, zero, 9
csetaddr cs1, cs0, t0
cseal ca1, ca0, cs1
lw a2, 0(ca1)
addi a3, zero, 99         # not reached

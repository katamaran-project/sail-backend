# Zbs: BCLR, BEXT, BINV, BSET and their immediate forms: bits 0 and 31, index taken mod 32.
addi a1, zero, 0
addi a2, zero, -1
li a3, 0x80000000
addi a4, zero, 31
addi a5, zero, 33         # bit 1
bclr a0, a2, a4           # 0x7fffffff
bext a0, a3, a4           # 1
bext a0, a3, zero         # 0
binv a0, a1, a5           # 2
bset a0, a1, a4           # 0x80000000
bclri a0, a3, 31          # 0
bexti a0, a2, 0           # 1
binvi a0, a2, 31          # 0x7fffffff
bseti a0, a1, 0           # 1
bset a0, a3, a5

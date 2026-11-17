# Compressed (C always on): C.ADDI with positive and negative immediates, C.NOP;
# a 32-bit instruction at an address 2 mod 4, between two 16-bit ones.
addi a1, zero, 5
c.addi a1, 1              # a1 = 6
addi a2, a1, 1            # at 2 mod 4: a2 = 7
c.addi a2, -32            # a2 = -25
c.nop

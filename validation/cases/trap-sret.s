# SRET in M without S-mode (misa.S = 0): illegal instruction (mcause 2).
addi a1, zero, 5
sret
addi a2, zero, 99         # not reached

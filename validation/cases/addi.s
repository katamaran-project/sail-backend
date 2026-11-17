# ADDI: positive, negative, extreme immediates, x0 as destination.
addi a0, zero, 5
addi a1, a0, -3
addi a2, zero, -2048
addi a3, a2, 2047
addi zero, a0, 1          # no effect
addi a0, a0, 0

# Zbkx: XPERM4, XPERM8: in-range and out-of-range indices (0, all ones, the sign bit).
li a1, 0x76543210
li a2, 0x01234567
addi a3, zero, -1
li a4, 0x80000000
xperm4 a0, a1, a2         # nibble indices 0-7 of a1
xperm4 a0, a1, a3         # all indices 15: out of range, 0
xperm4 a0, a2, a4
xperm8 a0, a1, a2         # byte indices 7, 6, 5, 4 out of range, 0x67 & 3 ...
li a5, 0x00010203
xperm8 a0, a1, a5         # bytes reversed
xperm8 a0, a1, a3         # 0
xperm8 a0, a1, zero       # byte 0 everywhere

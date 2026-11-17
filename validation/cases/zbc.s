# Zbc: CLMUL, CLMULH, CLMULR on 0, all ones, the sign bit, mixed values.
addi a1, zero, 0
addi a2, zero, -1
li a3, 0x80000000
li a4, 0x00000003
li a5, 0x12345678
clmul a0, a2, a1          # 0
clmul a0, a4, a4          # 5
clmulh a0, a2, a2
clmulr a0, a2, a2
clmul a0, a3, a4          # 0x80000000
clmulh a0, a3, a4         # 1
clmulr a0, a3, a3
clmul a0, a5, a2
clmulh a0, a5, a5
clmulr a0, a5, a4

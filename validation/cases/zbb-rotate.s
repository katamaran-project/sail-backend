# Zbb: ROL, ROR, RORI: amounts 0, 1, 31, 32 (taken mod 32), -1.
li a1, 0x80000001
addi a2, zero, 1
addi a3, zero, 31
addi a4, zero, 32
addi a5, zero, -1
rol a0, a1, a2            # 0x00000003
ror a0, a1, a2            # 0xc0000000
rol a0, a1, a3
ror a0, a1, a4            # amount 0
rol a0, a1, a5            # amount 31
ror a0, a1, zero
rori a0, a1, 0
rori a0, a1, 31
rori a0, a5, 16

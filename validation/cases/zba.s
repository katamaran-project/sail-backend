# Zba: SH1ADD, SH2ADD, SH3ADD on 0, all ones, the sign bit.
addi a1, zero, 0
addi a2, zero, -1
li a3, 0x80000000
sh1add a0, a1, a2         # -1
sh2add a0, a2, a2         # -4 + -1 = -5
sh3add a0, a3, a2         # sign bit shifted out: -1
sh1add a0, a3, a3         # 0x80000000
sh3add a0, a2, a1         # -8

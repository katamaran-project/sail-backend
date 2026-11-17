# Compressed integer instructions: C.LI, C.LUI, C.ADDI (edge immediates),
# C.SLLI, C.SRLI, C.SRAI (shift 1 and 31), C.ANDI (-1, 0, -32), C.SUB, C.XOR,
# C.OR, C.AND, C.MV, C.ADD (overflow); C.SRLI, C.SRAI with shamt[5] = 1 (Sail
# decodes them on RV32 and shifts by shamt[4:0]). Registers x8-x13 for the 3-bit forms.
c.li a0, 0
c.li a0, 31               # a0 = 31
c.li a1, -32              # a1 = -32
c.lui a2, 1               # a2 = 0x1000
c.lui a3, 31              # a3 = 0x1f000
c.lui s0, 0xfffe0         # s0 = 0xfffe0000 (sign extended from bit 17)
c.lui s1, 0xfffff         # s1 = 0xfffff000
c.addi a0, 31             # a0 = 62
c.addi a0, -32            # a0 = 30
c.addi a1, -1             # a1 = -33
c.slli a2, 1              # a2 = 0x2000
c.slli a0, 31             # a0 = 0
c.li a0, -1
c.srli a0, 1              # a0 = 0x7fffffff
c.srli s1, 31             # s1 = 1
c.srai s0, 31             # s0 = -1
c.srai a1, 1              # a1 = -17
c.li a0, 21
c.andi a0, -1             # a0 = 21
c.andi a0, 0              # a0 = 0
c.li a0, -1
c.andi a0, -32            # a0 = 0xffffffe0
c.li a1, 12
c.li a2, 10
c.sub a1, a2              # a1 = 2
c.sub a1, a2              # a1 = -8
c.xor a1, a2              # a1 = -8 ^ 10 = 0xfffffff2
c.or a2, a3               # a2 = 0x1f00a
c.and a3, a2              # a3 = 0x1f000
c.mv t0, a1               # t0 = 0xfffffff2
c.mv a0, a3
lui t1, 0x80000
c.mv t2, t1               # t2 = 0x80000000
c.add t2, t1              # overflow: t2 = 0
c.add t1, a3              # t1 = 0x8001f000
c.add ra, t1              # ra = 0x8001f000
.half 0x9005              # C.SRLI s0, 33: shamt[5] = 1 decodes on RV32 in Sail, shift by 1: s0 = 0x7fffffff
.half 0x9585              # C.SRAI a1, 33: a1 = 0xfffffff9

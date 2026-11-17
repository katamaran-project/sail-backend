# Zbb: SEXT.B, SEXT.H, ZEXT.H, ORC.B, REV8 on 0, all ones, the sign bit, mixed values.
addi a1, zero, 0
addi a2, zero, -1
li a3, 0x80000000
li a4, 0x12348086
sext.b a0, a4             # 0xffffff86
sext.h a0, a4             # 0xffff8086
zext.h a0, a2             # 0x0000ffff
sext.b a0, a3             # 0
orc.b a0, a1              # 0
orc.b a0, a3              # 0xff000000
orc.b a0, a4              # 0xffffffff
rev8 a0, a4               # 0x86803412
rev8 a0, a3               # 0x00000080
rev8 a0, a2

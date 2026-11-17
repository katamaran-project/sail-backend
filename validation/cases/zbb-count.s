# Zbb: CLZ, CTZ, CPOP on 0, all ones, the sign bit, 1, and a mixed value.
addi a1, zero, 0
addi a2, zero, -1
li a3, 0x80000000
addi a4, zero, 1
li a5, 0x00f0f000
clz a0, a1                # 32
ctz a0, a1                # 32
cpop a0, a1               # 0
clz a0, a2                # 0
ctz a0, a2                # 0
cpop a0, a2               # 32
clz a0, a3                # 0
ctz a0, a3                # 31
cpop a0, a3               # 1
clz a0, a4                # 31
ctz a0, a4                # 0
clz a0, a5                # 8
ctz a0, a5                # 12
cpop a0, a5               # 8

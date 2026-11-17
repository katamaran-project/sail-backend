# Zbb: ANDN, ORN, XNOR, MIN, MINU, MAX, MAXU on 0, all ones, the sign bit.
addi a1, zero, 0
addi a2, zero, -1
li a3, 0x80000000
li a4, 0x12345678
andn a0, a2, a4
andn a0, a4, a3
orn a0, a1, a4
orn a0, a3, a2            # 0x80000000
xnor a0, a4, a4           # -1
xnor a0, a3, a1
min a0, a3, a2            # INT_MIN
minu a0, a3, a2           # 0x80000000
max a0, a3, a1            # 0
maxu a0, a1, a2           # 0xffffffff
min a0, a4, a1
maxu a0, a4, a3

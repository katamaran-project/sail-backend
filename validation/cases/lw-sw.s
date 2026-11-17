# Loads and stores through a capability: word, half, byte, sign/zero extension,
# a misaligned word (misaligned access enabled on both sides).
cspecialr ca0, mtdc       # root memory capability
li a1, 0x80002000
csetaddr ca0, ca0, a1
li a2, 0xdeadbeef
sw a2, 0(ca0)
lw a3, 0(ca0)
lb a3, 3(ca0)             # 0xffffffde
lbu a3, 3(ca0)            # 0xde
lh a3, 2(ca0)             # 0xffffdead
lhu a3, 2(ca0)
sh a2, 8(ca0)
sb a2, 13(ca0)
lw a3, 8(ca0)
sw a2, 17(ca0)            # misaligned
lw a3, 17(ca0)
lw a3, 16(ca0)

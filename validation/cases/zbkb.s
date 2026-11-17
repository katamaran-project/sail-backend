# Zbkb: PACK, PACKH, BREV8, ZIP, UNZIP on 0, all ones, the sign bit, mixed values.
addi a1, zero, 0
addi a2, zero, -1
li a3, 0x80000000
li a4, 0x12345678
pack a0, a4, a2           # 0xffff5678
pack a0, a3, a4           # 0x56780000
packh a0, a4, a2          # 0x0000ff78
packh a0, a3, a1          # 0
brev8 a0, a4              # 0x482c6a1e
brev8 a0, a3              # 0x01000000
zip a0, a3
zip a0, a4
zip a0, a2
unzip a0, a3
unzip a0, a4
unzip a0, a1

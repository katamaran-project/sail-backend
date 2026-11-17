# LUI: low, high (sign bit), all ones, zero; then ADDI on top (li pattern).
lui a0, 0x12345
lui a1, 0x80000
lui a2, 0xfffff
lui a3, 0
lui a0, 0x12345
addi a0, a0, 0x678        # a0 = 0x12345678

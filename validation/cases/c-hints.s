# Compressed hints (execute nothing on RV32): C.NOP with an immediate, C.ADDI
# with immediate 0, C.LI/C.LUI/C.MV/C.ADD into x0, C.SLLI into x0 and with
# shift 0, C.SRLI/C.SRAI with shift 0. Every register keeps its value.
li a0, 0x12345678
li a1, -5
c.nop 1                   # C_NOP_HINT
c.nop -32                 # C_NOP_HINT
c.addi a0, 0              # C_ADDI_HINT
c.li zero, 5              # C_LI_HINT
c.lui zero, 1             # C_LUI_HINT
c.mv zero, a0             # C_MV_HINT
c.add zero, a1            # C_ADD_HINT
c.slli zero, 3            # C_SLLI_HINT: rd = x0
c.slli a0, 0              # C_SLLI_HINT: shift 0
c.srli a0, 0              # C_SRLI_HINT
c.srai a1, 0              # C_SRAI_HINT
c.mv zero, x16            # C_MV_HINT: x16 not read (RV32E)
c.add zero, x31           # C_ADD_HINT: x31 not read
c.li a2, 1

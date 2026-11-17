# Zicsr: CSRRW/CSRRS/CSRRC and immediate forms on mscratch, mtval, mstatus;
# reads of minstret and misa.
li a0, 0x12345678
csrrw a1, mscratch, a0    # a1 = 0, mscratch = a0
csrrs a2, mscratch, zero  # read
csrrwi a3, mscratch, 5
csrrc a2, mscratch, a3    # clear bits of 0x12345678 in 5
csrrsi a2, mscratch, 0x18
csrrw zero, mtval, a0
csrrs a1, mtval, zero
csrrs a1, minstret, zero
csrrs a1, misa, zero
csrrsi a1, mstatus, 8     # MIE
csrrci a1, mstatus, 8

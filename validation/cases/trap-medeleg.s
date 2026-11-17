# medeleg, like mideleg, mcounteren, menvcfg: not defined without U/S/N
# (misa.U = 0, not writable): illegal instruction.
li a0, -1
csrrw a1, 0x302, a0
addi a3, zero, 99            # not reached

# CSR write of mepc: CHERIoT's set_xret_target is internal_error
# ("unsupported"; MEPCC is written by CSpecialW), an assertion failure on both
# sides.
li a0, 0x80000100
csrrw a1, mepc, a0
addi a3, zero, 99            # not reached

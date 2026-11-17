# CSR write of mtvec: CHERIoT's set_mtvec is internal_error ("unsupported";
# MTCC is written by CSpecialW), an assertion failure on both sides.
li a0, 0x80000100
csrrw a1, mtvec, a0
addi a3, zero, 99            # not reached

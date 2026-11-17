# CSRW mepc: CHERIoT's set_xret_target is an internal error (MEPCC is written
# with CSpecialW only); the model stops.
li a0, 0x80000100
csrrw zero, mepc, a0
addi a2, zero, 99         # not reached

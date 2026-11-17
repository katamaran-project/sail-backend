# AUICGP: on null cgp (untagged result), on a memory capability (positive and
# negative immediate), on a bounded one (far: unrepresentable, tag cleared),
# on a sealed one (tag cleared).
# Capabilities in cgp, ca0-ca3, cs0, cs1.
auicgp ca0, 1             # cgp null at reset
cspecialr cgp, mtdc
li t0, 0x80002000
csetaddr cgp, cgp, t0
auicgp ca0, 1             # + 0x800
auicgp ca1, 0xfffff       # - 0x800
auicgp ca2, 0
addi t1, zero, 64
csetbounds cgp, cgp, t1
auicgp ca3, 0             # in bounds
auicgp ca3, 0x100         # + 0x80000: unrepresentable
cgettag t2, ca3
cspecialr cs0, mscratchc
addi t0, zero, 9
csetaddr cs1, cs0, t0
cseal cgp, cgp, cs1
auicgp ca3, 0             # sealed: tag cleared
cgettag t2, ca3

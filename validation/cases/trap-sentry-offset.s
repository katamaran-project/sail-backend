# CJALR through an inheriting sentry (otype 1) with a nonzero offset: seal
# violation (mcause 0x1c).
cspecialr cs0, mscratchc
addi t0, zero, 1
csetaddr cs1, cs0, t0
cla ct0, target
cseal ct0, ct0, cs1
cjalr cra, 4(ct0)
addi a2, zero, 99         # not reached
target:
addi a2, zero, 98         # not reached
addi a2, zero, 97         # not reached

# CJALR into cnull through an interrupt-disabling forward sentry (otype 2): a
# tail call needs an unsealed or inheriting target: seal violation
# (mcause 0x1c, mtval 0x3 << 5 | 5 = ct0, seal).
cspecialr cs0, mscratchc
addi t0, zero, 2
csetaddr cs1, cs0, t0
cla ct0, target
cseal ct0, ct0, cs1
cjalr cnull, ct0
addi a2, zero, 99         # not reached
target:
addi a2, zero, 98         # not reached

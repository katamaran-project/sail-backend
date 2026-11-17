# Fetch outside PCC bounds: CJALR into a capability bounded to two
# instructions, the third fetch faults (CHERI bounds violation, mtval 0x20 << 5
# | 1 = PCC); PCC's tag is cleared before it goes to MEPCC.
cla ct0, bounded
addi a0, zero, 8
csetbounds ct0, ct0, a0
cjalr zero, ct0
bounded:
addi a1, zero, 1
addi a2, zero, 2
addi a3, zero, 3          # outside the bounds: not reached

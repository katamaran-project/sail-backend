# A 32-bit instruction whose first parcel is the last one inside PCC bounds:
# CJR into a capability bounded to 6 bytes (two C.NOPs, then ADDI); fetching
# the ADDI's second parcel is a bounds violation.
cla ct0, bounded
addi a0, zero, 6
csetbounds ct0, ct0, a0
c.cjr t0
bounded:
c.nop
c.nop
addi a1, zero, 1          # second parcel outside the bounds: not executed

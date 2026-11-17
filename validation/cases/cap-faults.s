# Capability checks of CJALR, CLC, CSC (handle_cheri_reg_exception, mcause
# 0x1c, mtval = cause | reg << 5): untagged, sealed, sealed with an offset,
# sentry kind, permissions (execute, load, store, store-capability), bounds;
# then misaligned CLC/CSC (mcause 4, 6). The case's handler skips the
# instruction.
# limit: 400
cla ct0, skip
cspecialw mtcc, ct0
jal ra, sealed            # cra := return sentry (sealed)
sealed:
addi a1, zero, 5          # ca1 untagged
cjalr zero, ca1           # tag
cjalr zero, 4(ra)         # sealed, offset != 0
cjalr ra, ra              # call through a backward sentry
cspecialr ct2, mtdc       # data root: no execute
cjalr zero, ct2           # execute
li a0, 0x80000f00
csetaddr ct2, ct2, a0
addi a0, zero, 16
csetbounds ct2, ct2, a0   # [0x80000f00, +16)
clc ca2, 0(ca1)           # tag
clc ca2, 0(cra)           # sealed
csc ct2, 0(ca1)           # tag
csc ct2, 0(cra)           # sealed
li a0, 0xfdf              # all but load (bit 5)
candperm cs0, ct2, a0
clc ca2, 0(cs0)           # load
li a0, 0xffb              # all but store (bit 2)
candperm cs0, ct2, a0
csc ct2, 0(cs0)           # store
li a0, 0xfbf              # all but load/store capability (bit 6)
candperm cs0, ct2, a0
csc ct2, 0(cs0)           # store capability (tagged ct2)
csc ca1, 0(cs0)           # untagged ca1: allowed
clc ca2, 16(ct2)          # bounds
csc ct2, 16(ct2)          # bounds
clc ca2, 4(ct2)           # misaligned
csc ct2, 4(ct2)           # misaligned
clc ca2, 8(ct2)           # in bounds, aligned
j done
skip:
csrrs s1, mcause, zero
csrrs gp, mtval, zero
cspecialr ctp, mepcc
cincaddrimm ctp, ctp, 4
cspecialw mepcc, ctp
mret
done:

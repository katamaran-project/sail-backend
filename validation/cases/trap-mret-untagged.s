# MEPCC written with bit 0 of the address set: legalised (bit 0 cleared) with
# the tag cleared; MRET installs it as PCC, the fetch faults with a CHERI tag
# violation on PCC (mcause 0x1c, mtval 0x20 << 5 | 2), MTCC tagged.
cla ct0, there
cincaddrimm ct0, ct0, 1
cspecialw mepcc, ct0
mret
there:
addi a2, zero, 99         # not reached

# CSetBounds, CSetBoundsImm, CSetBoundsExact: small exact, large (rounded),
# exact on an inexact length (tag cleared), beyond the parent (tag cleared),
# on null; read back with CGetBase/CGetLen/CGetTop/CGetTag/CGetPerm.
# Integers in t0-t2, capabilities in ca0-ca3.
cspecialr ca0, mtdc
li t0, 0x80002000
csetaddr ca0, ca0, t0
addi t1, zero, 64
csetbounds ca1, ca0, t1
cgetbase t2, ca1
cgetlen t2, ca1
csetboundsimm ca2, ca0, 16
cgettop t2, ca2
li t1, 0x12345
csetbounds ca2, ca0, t1   # rounded up
cgetlen t2, ca2
cgetbase t2, ca2
csetboundsexact ca3, ca0, t1  # not exact: tag cleared
cgettag t2, ca3
addi t1, zero, 128
csetbounds ca3, ca1, t1   # beyond ca1: tag cleared
cgettag t2, ca3
csetbounds ca3, cnull, t1
cgetperm t2, ca1

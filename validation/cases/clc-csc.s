# CSC/CLC: store and load capabilities (tag preserved), overwrite half with SW
# (tag cleared), load an untagged capability, store null.
# Integers in t0-t2, capabilities in ca0-ca3.
cspecialr ca0, mtdc
li t0, 0x80002000
csetaddr ca0, ca0, t0
addi t1, zero, 64
csetbounds ca1, ca0, t1
csc ca1, 0(ca0)
csc ca0, 8(ca0)
clc ca2, 0(ca0)
clc ca3, 8(ca0)
cgettag t2, ca3
sw zero, 4(ca0)           # clears the granule's tag
clc ca3, 0(ca0)
cgettag t2, ca3
csc cnull, 8(ca0)
clc ca3, 8(ca0)
cgettag t2, ca3

# CIncAddr, CIncAddrImm, CSetAddr: on the root capability, on a bounded one
# (in bounds, out of bounds but representable, unrepresentable: tag cleared),
# on null (stays untagged).
# Integer and capability registers are the same (a0 = ca0): integers in t0-t2.
cspecialr ca0, mtdc
li t0, 0x80002000
csetaddr ca0, ca0, t0
addi t1, zero, 0x100
cincaddr ca1, ca0, t1
cincaddrimm ca1, ca1, -16
addi t1, zero, 64
csetbounds ca2, ca0, t1   # [0x80002000, +64)
cincaddrimm ca3, ca2, 32
cincaddrimm ca3, ca2, 80  # out of bounds, representable
li t1, 0x10000
cincaddr ca3, ca2, t1     # far out: unrepresentable, tag cleared
cincaddrimm ca3, cnull, 4
csetaddr ca3, cnull, t0

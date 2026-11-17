# Setup of the parcel sweep (validate.md, "All 16-bit parcels"), after the
# prologue: the fixed state from which every 16-bit parcel runs one step, at
# `slot` (2 mod 4). Registers with distinct values: memory capabilities in
# x2 and x8-x11 (data written behind them), a bounded one in x12, null in
# x13, a read-only one in x14, an executable one in x15, a return sentry in
# x1, integers in x3-x7; x16-x31 zero.
cspecialr ca0, mtdc       # root memory capability
li t0, 0x80002000
csetaddr cs0, ca0, t0
li t0, 0x80002100
csetaddr cs1, ca0, t0
li t0, 0x80002300
csetaddr ca1, ca0, t0
li t0, 0x80003000
csetaddr csp, ca0, t0
li t0, 0x80002400
csetaddr ca2, ca0, t0
addi t0, zero, 64
csetbounds ca2, ca2, t0   # 64 bytes: larger offsets fault
li t0, 0x80002600
csetaddr ca4, ca0, t0
csc cs1, 0(ca4)           # data behind it, before the store permission goes
addi t0, zero, 0x6b       # GL, LG, LM, LD, MC: no store (SD)
candperm ca4, ca4, t0
cla ca5, land             # executable
li t0, 0x80002200
csetaddr ca0, ca0, t0
li gp, 0x12345678
li tp, -0x5555
li t0, 0x7fffffff
li t1, 0x80000000
li t2, 0x00000ff0
csc cs1, 0(cs0)           # data behind the capabilities
sw gp, 8(cs0)
csc cs0, 0(cs1)
sw tp, 12(cs1)
csc csp, 16(ca0)
sw t1, 0(ca1)
csc ca5, 0(csp)
sw t0, 8(csp)
csc cs0, 248(csp)
sw gp, 252(csp)
csc ca1, 504(csp)
mv a3, zero               # null
jal ra, linked            # ra: return sentry
linked:
.align 4
c.nop
slot:
c.nop                     # replaced by each parcel
land:
c.nop

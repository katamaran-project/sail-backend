# CAndPerm (keep all, drop some, drop all, odd combinations re-encoded by
# CGetHigh), CRRL, CRAM (small, large, rounding to 2^32), CSetBoundsRoundDown
# (exact, rounded down, zero, beyond the parent, on null).
# Integers in t0-t2, capabilities in ca0-ca3.
# Permission bits: GL 0, LG 1, SD 2, LM 3, SL 4, LD 5, MC 6, SR 7, EX 8, US 9, SE 10, U0 11.
cspecialr ca0, mtdc
li t0, 0x80002000
csetaddr ca0, ca0, t0
addi t1, zero, -1
candperm ca1, ca0, t1     # all kept
cgetperm t2, ca1
addi t1, zero, 0x7fb      # drop SD
candperm ca1, ca0, t1
cgetperm t2, ca1
cgethigh t2, ca1
addi t1, zero, 0x7df      # drop LD, keep SD and MC
candperm ca2, ca0, t1
cgetperm t2, ca2
cgethigh t2, ca2
addi t1, zero, 0x001      # GL only
candperm ca2, ca0, t1
cgetperm t2, ca2
cgethigh t2, ca2
candperm ca2, ca0, zero   # none
cgetperm t2, ca2
cgettag t2, ca2
candperm ca2, cnull, t1
cla ca3, here             # executable
here:
li t1, 0xf7f              # drop SR
candperm ca3, ca3, t1
cgetperm t2, ca3
cgethigh t2, ca3
addi t1, zero, 0x5ff      # drop U0, US
candperm ca3, ca3, t1
cgetperm t2, ca3
addi t0, zero, 13
crrl t2, t0
cram t2, t0
li t0, 0x12345
crrl t2, t0
cram t2, t0
li t0, 0xffffffff
crrl t2, t0               # rounds up to 2^32: 0
cram t2, t0
crrl t2, zero
cram t2, zero
addi t1, zero, 64
csetboundsrounddown ca1, ca0, t1    # exact
cgetbase t2, ca1
cgetlen t2, ca1
cgettag t2, ca1
li t1, 0x12345
csetboundsrounddown ca2, ca0, t1    # rounded down
cgetlen t2, ca2
cgettag t2, ca2
csetboundsrounddown ca2, ca0, zero  # zero length
cgetlen t2, ca2
addi t1, zero, 128
csetboundsrounddown ca3, ca1, t1    # beyond ca1: tag cleared
cgettag t2, ca3
cgetlen t2, ca3
csetboundsrounddown ca3, cnull, t1
cgettag t2, ca3

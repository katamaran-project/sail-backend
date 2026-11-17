# PCC without access-system-registers (CAndPerm clears it, CJALR installs it):
# the counters (minstret, mcycle) read, every other CSR access, a counter write,
# CSpecialR and MRET trap (CHERI exception, cause 0x18) to the case's own
# handler, which skips the instruction (MEPCC + 4) and returns.
cla ct0, skip
cspecialw mtcc, ct0
cla cs0, back             # with ASR
cla ct0, noasr
li a0, 0xf7f              # all permissions but ASR (bit 7)
candperm ct0, ct0, a0
cjalr zero, ct0
noasr:
csrrs a1, minstret, zero  # allowed
csrrs a1, mcycle, zero    # allowed
csrrs a1, mstatus, zero   # trap
csrrw zero, minstret, a0  # trap: write
cspecialr ca2, mtdc       # trap: mtval reg 0x20 | 29
mret                      # trap: ext_fail_xret_priv
csrrs a3, minstreth, zero # allowed
cjalr zero, cs0           # back, with ASR
skip:
csrrs a1, mcause, zero
csrrs a2, mtval, zero
cspecialr ct1, mepcc
cincaddrimm ct1, ct1, 4
cspecialw mepcc, ct1
mret
back:
addi a3, zero, 3

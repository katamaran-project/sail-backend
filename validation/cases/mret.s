# MRET without a trap, to MEPCC written with CSpecialW: MIE := MPIE, MPIE := 1,
# MPP := M (no U). Twice: MPIE = 0 (MIE 1 -> 0), then MPIE = 1 (MIE 0 -> 1).
cla ct0, there
cspecialw mepcc, ct0
csrrsi zero, mstatus, 8   # MIE = 1
li a0, 0x80               # MPIE
csrrc zero, mstatus, a0   # MPIE = 0
mret                      # MIE = 0, MPIE = 1
addi a2, zero, 99         # skipped
there:
csrrs a1, mstatus, zero
cla ct0, back
cspecialw mepcc, ct0
mret                      # MIE = 1 (MPIE), MPIE = 1
addi a2, zero, 98         # skipped
back:
csrrs a1, mstatus, zero
csrrs a3, mepc, zero

# ECALL (mcause 11) and EBREAK (mcause 3, mtval = pc) from M to the case's own
# handler (MTCC), which reads mcause, mepc, mtval, mstatus, mtvec and returns
# past the trap with MRET (MEPCC + 4): mstatus.MIE saved in MPIE by the trap,
# restored by MRET, MPIE = 1, MPP = M (no U).
cla ct0, handler
cspecialw mtcc, ct0
csrrsi zero, mstatus, 8   # MIE = 1
ecall
csrrs a3, mstatus, zero   # MIE = 1 again
csrrci zero, mstatus, 8   # MIE = 0
ebreak
csrrs a3, mstatus, zero
j done
handler:
csrrs a0, mcause, zero
csrrs a1, mepc, zero      # MEPCC.address
csrrs a2, mstatus, zero   # MIE = 0, MPIE = old MIE
csrrs s0, mtvec, zero     # MTCC.address
csrrs s1, mtval, zero
cspecialr ct1, mepcc
cincaddrimm ct1, ct1, 4
cspecialw mepcc, ct1
mret
done:
addi a3, zero, 3

# limit: 300
# Reserved 16-bit encodings trap illegal (mcause 2) to the case's own handler,
# which counts them in s1 and returns past the parcel (MEPCC + 2):
# all zero (C.ILLEGAL), C.ADDI4SPN and C.ADDI16SP with immediate 0, C.LUI with
# immediate 0, C.CLCSP into cnull, C.SLLI with shamt[5] = 1, C.JR x0, Q0 100,
# the F/D slots (C.FLD, C.FLDSP, C.FSD, C.FSDSP), the RV64 C.SUBW, C.ADDW and
# the two reserved Q1 100 1 11 slots.
cla ct0, handler
cspecialw mtcc, ct0
c.li s1, 0
.half 0x0000              # C.ILLEGAL
.half 0x0004              # C.ADDI4SPN cs1, 0
.half 0x6101              # C.ADDI16SP 0
.half 0x6501              # C.LUI a0, 0
.half 0x6002              # C.CLCSP cnull
.half 0x1502              # C.SLLI a0, 32
.half 0x8002              # C.JR x0
.half 0x8000              # Q0 100
.half 0x2000              # C.FLD
.half 0x2002              # C.FLDSP
.half 0xa000              # C.FSD
.half 0xa002              # C.FSDSP
.half 0x9c01              # C.SUBW
.half 0x9c21              # C.ADDW
.half 0x9c41              # reserved
.half 0x9c61              # reserved
c.j done
.align 4                  # MTCC: 4-byte aligned
handler:
c.addi s1, 1
csrrs a0, mcause, zero
csrrs a1, mtval, zero
cspecialr ct1, mepcc
cincaddrimm ct1, ct1, 2
cspecialw mepcc, ct1
mret
done:
c.li a2, 1

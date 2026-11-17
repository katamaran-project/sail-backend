# Loads and stores outside RAM and devices, through the data root: access
# faults (mcause 5 load, 7 store/AMO) for LW, SW, LB, CLC, CSC. The case's
# handler skips the instruction.
cla ct0, skip
cspecialw mtcc, ct0
cspecialr ct2, mtdc
li a0, 0x40000000
csetaddr ct2, ct2, a0
lw a1, 0(ct2)
sw a1, 0(ct2)
lb a1, 3(ct2)
clc ca2, 8(ct2)
csc ct2, 8(ct2)
li a0, 0x02000000         # CLINT msip: a word store is fine, a byte store faults
csetaddr ct2, ct2, a0
sb zero, 0(ct2)
j done
skip:
csrrs s1, mcause, zero
csrrs gp, mtval, zero
cspecialr ctp, mepcc
cincaddrimm ctp, ctp, 4
cspecialw mepcc, ctp
mret
done:

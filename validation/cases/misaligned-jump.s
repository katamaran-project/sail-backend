# Jump targets not 4-byte aligned (no RVC): taken BEQ, CJAL, CJALR trap with
# a misaligned fetch address (mcause 0, mtval = target) before jumping; a
# not-taken BEQ does not; CJALR clears bit 0 of the target. The case's
# handler skips the instruction.
cla ct0, skip
cspecialw mtcc, ct0
addi a0, zero, 1
beq zero, zero, 6         # taken: misaligned
beq zero, a0, 6           # not taken
jal t1, 6                 # misaligned
cla ct2, next
cjalr zero, 2(ct2)        # misaligned
cla ct2, next
cjalr zero, 1(ct2)        # bit 0 cleared: to next
next:
addi a3, zero, 3
j done
skip:
csrrs s1, mcause, zero
csrrs gp, mtval, zero
cspecialr ctp, mepcc
cincaddrimm ctp, ctp, 4
cspecialw mepcc, ctp
mret
done:

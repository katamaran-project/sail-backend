# Jump targets 2 mod 4 (C on: no misaligned-fetch trap): taken BEQ, CJAL and
# CJALR reach them; a not-taken BEQ falls through; CJALR clears bit 0 of the
# target. Each target adds 1 to a0 (a0 = 5 at the end); a1 stays 0.
addi a0, zero, 1
beq zero, zero, half1     # taken: to 2 mod 4
addi a1, zero, 99         # skipped
c.nop                     # skipped
half1:
c.addi a0, 1
beq zero, a0, half1       # not taken
jal t1, half2             # to 2 mod 4, link in ct1
c.nop                     # skipped
half2:
c.addi a0, 1
cla ct2, word3
cjalr zero, 2(ct2)        # to word3 + 2
word3:
c.nop                     # skipped
c.addi a0, 1
cla ct2, word4
cjalr zero, 1(ct2)        # bit 0 cleared: to word4
addi a1, zero, 99         # skipped
word4:
c.addi a0, 1

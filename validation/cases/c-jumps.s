# Compressed control flow: C.J forward and backward (RISCV_JAL, reached only
# this way), C.BEQZ/C.BNEZ taken and not, forward and backward, C.CJAL into a
# subroutine (cra), C.CJALR through ct0, C.CJR back through cra and to a
# label; targets at 0 and 2 mod 4. a0 counts the blocks reached (8).
c.li a0, 0
c.li s0, 0
c.j f1                    # forward
c.li a0, 15               # skipped
f1:
c.addi a0, 1
c.beqz s0, f2             # taken, forward
c.li a0, 15               # skipped
f2:
c.addi a0, 1
c.bnez s0, f2             # not taken
c.beqz a0, f2             # not taken
c.li s1, 3
loop:
c.addi a0, 1
c.addi s1, -1
c.bnez s1, loop           # taken backward twice: a0 = 5
c.j over
back:
c.addi a0, 1
c.j after
over:
c.j back                  # backward
after:
c.cjal sub1               # cra: return sentry
cla ct0, sub2
c.cjalr t0
cla ct1, end
c.cjr t1
c.li a0, 15               # skipped
sub1:
c.addi a0, 1
c.cjr ra
sub2:
c.addi a0, 1
c.cjr ra
end:
c.li a1, 1

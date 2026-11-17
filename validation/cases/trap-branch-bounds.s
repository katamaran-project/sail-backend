# Taken branch to a target outside PCC bounds: the branch retires (no control
# check in CHERIoT), the fetch at the target faults (CHERI bounds violation).
cla ct0, bounded
addi a0, zero, 8
csetbounds ct0, ct0, a0
cjalr zero, ct0
bounded:
addi a1, zero, 1
beq zero, zero, out
addi a2, zero, 99         # not reached
out:
addi a3, zero, 3          # outside the bounds: not reached

# Branches: BEQ not taken and taken (forward, backward), BNE, BLT/BGEU.
addi a0, zero, 1
beq a0, zero, bad         # not taken
addi a1, zero, 2
beq a1, a1, fwd           # taken forward
bad:
addi a2, zero, 99         # skipped
fwd:
addi a3, zero, 3
back:
addi a3, a3, -1
bne a3, zero, back        # taken backward twice, then not
blt a1, a0, bad           # 2 < 1: not taken
bgeu a1, a0, done         # taken
addi a2, zero, 98         # skipped
done:
addi a0, zero, 0

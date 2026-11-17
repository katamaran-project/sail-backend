# Prologue of every case (validate.md): MTCC := PCC with address `trap`, so a
# trap jumps to the handler in the epilogue, which ends the run via tohost.
# Reserved for prologue and epilogue: c14 (a4), c15 (a5).
cla c15, trap             # auipcc c15, 0; cincaddrimm c15, c15, trap - .
cspecialw mtcc, c15

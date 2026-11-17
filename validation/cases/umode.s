# U-mode: mstatus.MPP = 0, then MRET enters User although misa.U = 0 (the model
# does not legalise MPP). The next step stops the model: dispatchInterrupt
# asserts the privilege is Machine when U-mode is absent.
li a0, 0x1800
csrrc zero, mstatus, a0   # MPP = User
cla ct0, user
cspecialw mepcc, ct0
mret                      # to U; MPP := M
user:
addi a1, zero, 1          # not reached

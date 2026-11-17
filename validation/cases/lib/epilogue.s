# Epilogue of every case (validate.md).
# pass: htif exit code 0 (tohost = 1, then tohost+4 = 0 completes the command).
pass:
cspecialr c15, mtdc       # root memory capability
lui a4, 0x80001           # tohost
csetaddr c15, c15, a4
addi a4, zero, 1
sw a4, 0(c15)
sw zero, 4(c15)
# trap: MTCC points here (prologue); htif exit code 1 (tohost = 3).
trap:
cspecialr c15, mtdc
lui a4, 0x80001
csetaddr c15, c15, a4
addi a4, zero, 3
sw a4, 0(c15)
sw zero, 4(c15)

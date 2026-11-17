# ADDI, ADD, SW; the rest gets a capability for the store and ends the run via htif.
addi a1, zero, 5          # x11 = 5
add  a2, a1, a1           # x12 = 10
cspecialr ca0, mtdc       # c10 = MTDC (root memory capability at reset)
lui  a3, 0x80001          # x13 = 0x80001000 (tohost)
csetaddr ca0, ca0, a3     # c10.address = tohost
sw   a2, 0x100(ca0)       # mem[0x80001100] = 10
addi a4, zero, 1
sw   a4, 0(ca0)           # tohost[31:0] = 1 (exit code 0)
sw   zero, 4(ca0)         # tohost[63:32] = 0: htif command complete, emulator exits

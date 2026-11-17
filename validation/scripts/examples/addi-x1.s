# ADDI x1, x0, 5 at the entry, as Rocq's DumpADDI; the rest ends the run via htif.
addi x1, x0, 5            # 0x00500093
cspecialr ca0, mtdc
lui  a3, 0x80001
csetaddr ca0, ca0, a3
addi a4, zero, 1
sw   a4, 0(ca0)
sw   zero, 4(ca0)

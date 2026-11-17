# Software interrupt: a CLINT msip store sets mip.MSI (pending while mie.MSIE
# or mstatus.MIE is clear); with both set, the next step takes it (mcause
# 0x80000003, handle_interrupt) and jumps to MTCC, the epilogue's handler.
cspecialr ca0, mtdc
li a1, 0x2000000
csetaddr ca0, ca0, a1
addi a2, zero, 1
sw a2, 0(ca0)                # msip = 1
lw a3, 0(ca0)
csrrsi zero, mie, 8          # MSIE
csrrs a3, mip, zero
csrrsi zero, mstatus, 8      # MIE: interrupt before the next instruction
addi a3, zero, 99            # not reached

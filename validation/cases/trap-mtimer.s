# Timer interrupt: CLINT mtimecmp stores (hi, lo) run clint_dispatch: mip.MTI
# is set once mtimecmp <= mtime (0: runs have no clock ticks); with mie.MTIE
# and mstatus.MIE set, the next step takes it (mcause 0x80000007).
cspecialr ca0, mtdc
li a1, 0x2004000
csetaddr ca0, ca0, a1
addi a2, zero, -1
sw a2, 4(ca0)                # mtimecmp hi = ~0: MTI clear
sw zero, 0(ca0)
csrrs a3, mip, zero
sw zero, 4(ca0)              # mtimecmp = 0 <= mtime: MTI
csrrs a3, mip, zero
addi a2, zero, 0x80
csrrs zero, mie, a2          # MTIE
csrrsi zero, mstatus, 8      # MIE: interrupt before the next instruction
addi a3, zero, 99            # not reached

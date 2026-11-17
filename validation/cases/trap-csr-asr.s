# CSR access with ASR (access-system-registers) cleared in PCC: reads of
# minstret and mcycle allowed, a write of mscratch traps (CHERI
# AccessSystemRegsViolation on PCC; ext_check_CSR_fail).
cla ca0, noasr
li a1, -129                  # all permissions but ASR (bit 7)
candperm ca0, ca0, a1
cjalr zero, ca0
noasr:
csrrs a2, minstret, zero
csrrs a2, mcycle, zero
csrrw a2, mscratch, a1
addi a3, zero, 99            # not reached

# C.ADDI a1, 1 (0x0585) then C.NOP (0x0001): compressed, executed (C always on).
# Never reaches tohost: run with --inst-limit.
.word 0x00010585

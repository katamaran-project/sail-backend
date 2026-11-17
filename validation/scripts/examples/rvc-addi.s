# C.ADDI a1, 1 (0x0585) then C.NOP (0x0001): illegal instruction with --disable-compressed.
# Never reaches tohost: run with --inst-limit.
.word 0x00010585

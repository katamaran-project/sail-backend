# Compressed absent (C.ADDI a1, 1 = 0x0585, then C.NOP): illegal instruction on both
# sides (harness sys_enable_rvc = false; emulator --disable-compressed), MTCC handler.
addi a1, zero, 5
.word 0x00010585
addi a2, zero, 99         # not reached

# Zb* absent (Zbb ANDN a0, a1, a2): illegal instruction on both sides (scope.toml
# haveZb* = false; emulator built without Zb*), MTCC handler.
addi a1, zero, 5
.word 0x40c5f533
addi a2, zero, 99         # not reached

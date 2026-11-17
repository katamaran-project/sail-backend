# RV32E: C.MV from x16. Expected divergence (divergences.toml): µSail fails,
# the emulator traps illegal (mcause 2).
c.li a1, 5
c.mv a0, x16
c.li a2, 7                # not reached

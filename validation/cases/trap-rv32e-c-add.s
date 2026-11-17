# RV32E: C.ADD into x16. Expected divergence (divergences.toml): µSail fails,
# the emulator traps illegal (mcause 2).
c.li a1, 5
c.add x16, a1
c.li a2, 7                # not reached

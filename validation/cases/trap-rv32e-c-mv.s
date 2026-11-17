# RV32E: C.MV from x16: traps illegal (mcause 2), as trap-rv32e-rs.
c.li a1, 5
c.mv a0, x16
c.li a2, 7                # not reached

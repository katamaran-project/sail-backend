# RV32E: C.ADD into x16: traps illegal (mcause 2), as trap-rv32e-rd.
c.li a1, 5
c.add x16, a1
c.li a2, 7                # not reached

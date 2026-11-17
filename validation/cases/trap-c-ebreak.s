# C.EBREAK: breakpoint trap (mcause 3, mtval = pc).
c.li a0, 1
c.ebreak
c.li a0, 2                # not reached

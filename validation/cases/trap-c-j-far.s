# C.J with the most negative offset (-2048) from the start of RAM: the target
# is below RAM, the fetch faults (access fault, mcause 1).
c.li a0, 1
c.j -2048
c.li a0, 2                # not reached

# C.LW, C.SW (offsets 0 and 124), C.LWSP, C.SWSP (0 and 252) through
# capabilities in cs0 and csp; a misaligned C.LW (2 mod 4, allowed).
cspecialr cs0, mtdc
li t0, 0x80002000
csetaddr cs0, cs0, t0
cmove csp, cs0
cincaddrimm csp, csp, 512
li a0, 0x11223344
li a1, -2
c.sw a0, 0(s0)
c.sw a1, 124(s0)
c.lw a2, 0(s0)            # a2 = 0x11223344
c.lw a3, 124(s0)          # a3 = -2
c.swsp a0, 0(sp)
c.swsp a1, 252(sp)
c.swsp s0, 8(sp)          # integer store of a capability register: its address
c.lwsp t1, 0(sp)          # t1 = 0x11223344
c.lwsp t2, 252(sp)        # t2 = -2
c.lwsp ra, 8(sp)          # ra = 0x80002000
cincaddrimm cs1, cs0, 2
c.lw a2, 0(s1)            # misaligned: a2 = bytes 2..5 = 0x00001122

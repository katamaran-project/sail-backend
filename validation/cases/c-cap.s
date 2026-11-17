# CHERIoT compressed capability instructions: C.CLC, C.CSC (offsets 0 and 248),
# C.CLCSP, C.CSCSP (0 and 504), C.CIncAddr16CSP (16, 496, -512),
# C.CIncAddr4CSPN (4, 1020). Tags kept through memory.
cspecialr cs0, mtdc
li t0, 0x80002000
csetaddr cs0, cs0, t0
addi t1, zero, 64
csetbounds cs1, cs0, t1   # cs1: bounded, tagged
cmove csp, cs0
c.csc s1, 0(s0)
c.csc s0, 248(s0)
c.clc a0, 0(s0)           # ca0 = cs1
c.clc a1, 248(s0)         # ca1 = cs0
c.cscsp s1, 0(sp)         # overwrites cs1 at 0
c.cscsp a1, 504(sp)
c.clcsp t1, 504(sp)       # ct1 = cs0
c.clcsp ra, 0(sp)         # cra = cs1
cgettag t2, ct1
c.cincaddr16csp sp, 16    # csp = 0x80002010
c.cincaddr16csp sp, 496   # csp = 0x80002200
c.cincaddr16csp sp, -512  # csp = 0x80002000
c.cincaddr4cspn a2, sp, 4      # ca2 = 0x80002004
c.cincaddr4cspn a3, sp, 1020   # ca3 = 0x800023fc

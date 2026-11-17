# CSeal, CUnseal: data otype 9 by the sealing root (MScratchC); refused (tag
# cleared): otype 8, sentry otype on data, already sealed, authority without
# SE/US, untagged, sealed, out of its bounds, wrong otype; CUnseal of an
# unsealed cap, authority without GL. Sealed operands to CIncAddrImm, CSetAddr,
# CSetBounds, CAndPerm (all ones, GL only cleared, other), CGetType, CGetBase.
# Integers in t0-t2, capabilities in ca0-ca3, cs0 (sealing root), cs1 (authority).
cspecialr ca0, mtdc
li t0, 0x80002000
csetaddr ca0, ca0, t0
addi t1, zero, 64
csetbounds ca0, ca0, t1   # data capability [0x80002000, +64)
cspecialr cs0, mscratchc
addi t0, zero, 9
csetaddr cs1, cs0, t0
cseal ca1, ca0, cs1       # sealed, otype 9
cgettype t2, ca1
cgettag t2, ca1
cunseal ca2, ca1, cs1     # unsealed, tagged
cgettype t2, ca2
cgettag t2, ca2
cseqx t2, ca2, ca0
addi t0, zero, 8
csetaddr cs1, cs0, t0
cseal ca2, ca0, cs1       # otype 8: refused
cgettag t2, ca2
addi t0, zero, 3
csetaddr cs1, cs0, t0
cseal ca2, ca0, cs1       # sentry otype on data: refused
cgettag t2, ca2
addi t0, zero, 9
csetaddr cs1, cs0, t0
cseal ca2, ca1, cs1       # already sealed: refused
cgettag t2, ca2
li t1, 0xbff              # drop SE
candperm ca3, cs1, t1
cseal ca2, ca0, ca3       # refused
cgettag t2, ca2
li t1, 0xdff              # drop US
candperm ca3, cs1, t1
cunseal ca2, ca1, ca3     # refused
cgettag t2, ca2
addi t1, zero, -2         # drop GL
candperm ca3, cs1, t1
cunseal ca2, ca1, ca3     # unsealed, without GL
cgetperm t2, ca2
cgettag t2, ca2
ccleartag ca3, cs1
cseal ca2, ca0, ca3       # untagged authority: refused
cgettag t2, ca2
cunseal ca2, ca1, ca3     # refused
cgettag t2, ca2
cseal ca3, cs1, cs1       # sealed authority (otype 9)
cseal ca2, ca0, ca3       # refused
cgettag t2, ca2
cunseal ca2, ca1, ca3     # refused
cgettag t2, ca2
addi t1, zero, 1
csetbounds ca3, cs1, t1   # [9, 10)
addi t0, zero, 10
csetaddr ca3, ca3, t0     # address 10: out of bounds
cseal ca2, ca0, ca3       # refused
cgettag t2, ca2
csetaddr ca3, cs0, t0
csetbounds ca3, ca3, t1   # [10, 11)
cunseal ca2, ca1, ca3     # otype 9 not in bounds: refused
cgettag t2, ca2
cunseal ca2, ca0, cs1     # unsealed operand: refused
cgettag t2, ca2
cincaddrimm ca2, ca1, 4   # sealed operands: tag cleared
cgettag t2, ca2
csetaddr ca2, ca1, t0
cgettag t2, ca2
csetbounds ca2, ca1, t1
cgettag t2, ca2
addi t1, zero, -1
candperm ca2, ca1, t1     # all ones: kept
cgettag t2, ca2
addi t1, zero, -2
candperm ca2, ca1, t1     # GL only cleared: kept
cgettag t2, ca2
cgetperm t2, ca2
addi t1, zero, -5
candperm ca2, ca1, t1     # SD cleared: tag cleared
cgettag t2, ca2
cgetbase t2, ca1
cgetaddr t2, ca1

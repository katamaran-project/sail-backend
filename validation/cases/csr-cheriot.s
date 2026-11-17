# CHERIoT CSRs: mccsr (d, e read 1, writes ignored); mshwmb, mshwm (writes
# rounded down to 16 bytes); a store in [mshwmb, mshwm) lowers mshwm to its
# address rounded down, one outside does not; mtvec reads MTCC's address.
csrrs a1, mccsr, zero
csrrw a1, mccsr, zero        # ignored
csrrs a1, mccsr, zero
li a0, 0x80002017
csrrw a1, mshwmb, a0         # 0x80002010
li a0, 0x8000210f
csrrw a1, mshwm, a0          # 0x80002100
csrrs a1, mshwm, zero
csrrs a1, mshwmb, zero
cspecialr ca2, mtdc
li a3, 0x80002088
csetaddr ca2, ca2, a3
sw a3, 0(ca2)                # in range: mshwm = 0x80002080
li a3, 0x80002200
csetaddr ca2, ca2, a3
sw a3, 0(ca2)                # above mshwm: unchanged
li a3, 0x80002000
csetaddr ca2, ca2, a3
sb a3, 0(ca2)                # below mshwmb: unchanged
li a3, 0x8000201f
csetaddr ca2, ca2, a3
sb a3, 0(ca2)                # in range: mshwm = 0x80002010
csrrs a1, mshwm, zero
csrrs a1, mtvec, zero        # MTCC.address

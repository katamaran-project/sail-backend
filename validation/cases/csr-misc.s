# Zicsr side effects: misa writes (legalize_misa: F, D not writable on this
# configuration; C is, and clearing it asks ext_veto_disable_C), mie and mip
# legalization (mip: S bits only), mstatus all ones (legalize_mstatus),
# mcountinhibit.IR stops minstret, seed CSR 0x015 (read into x0: the
# emulator's entropy is random; write ignored), mepc read (MEPCC's address,
# pc_alignment_mask), mcycle(h)/minstreth writes, machine ID reads.
li a0, -1
csrrw a1, misa, a0
csrrw a1, misa, zero
csrrs a1, misa, zero
csrrw a1, mie, a0            # MEI MTI MSI SEI STI SSI
csrrw a1, mie, zero
csrrw a1, mip, a0            # SEI STI SSI
csrrw a1, mip, zero
csrrw a1, mstatus, a0
csrrw a1, mstatus, zero
csrrwi a1, mcountinhibit, 31 # IR, CY
csrrs a1, mcountinhibit, zero
addi a2, zero, 1             # minstret unchanged
csrrwi a1, mcountinhibit, 0
csrrs a1, minstret, zero
csrrw zero, 0x015, zero      # seed
cspecialr ca2, mtdc
li a3, 0x80000102
csetaddr ca2, ca2, a3
cspecialw mepcc, ca2
csrrs a1, mepc, zero         # bit 1 masked: misa.C cleared above
csrrw a1, mcycle, a0
csrrw a1, mcycleh, a0
csrrs a1, mcycle, zero
csrrw a1, minstreth, a0
csrrs a1, mhartid, zero
csrrs a1, mvendorid, zero
csrrs a1, marchid, zero
csrrs a1, mimpid, zero

# MMIO: CLINT loads and stores (msip, mtimecmp lo/hi, mtime lo/hi; msip
# pending but not enabled); UART (line status load, data store to the
# terminal, DLAB set, data store dropped, DLAB clear, store to an ignored
# offset); HTIF (tohost loads, putchar: device 1, cmd 1, port reset).
cspecialr ca0, mtdc
li a1, 0x2000000
csetaddr ca0, ca0, a1
lw a2, 0(ca0)                # msip
addi a2, zero, 1
sw a2, 0(ca0)                # msip = 1: mip.MSI
csrrs a3, mip, zero
sw zero, 0(ca0)
csrrs a3, mip, zero
li a1, 0x2004000
csetaddr ca0, ca0, a1
li a2, 0x12345678
sw a2, 0(ca0)                # mtimecmp lo
sw a2, 4(ca0)                # mtimecmp hi
lw a3, 0(ca0)
lw a3, 4(ca0)
li a1, 0x200bff8
csetaddr ca0, ca0, a1
lw a3, 0(ca0)                # mtime lo
lw a3, 4(ca0)                # mtime hi
li a1, 0x10000000
csetaddr ca0, ca0, a1
lw a3, 20(ca0)               # line status: 0x20
addi a2, zero, 0x41
sw a2, 0(ca0)                # 'A' (plat_term_write)
addi a2, zero, 0x80
sw a2, 12(ca0)               # line control: DLAB = 1
sw a2, 0(ca0)                # dropped
sw zero, 12(ca0)             # DLAB = 0
sb a2, 4(ca0)                # ignored offset
li a1, 0x80001000
csetaddr ca0, ca0, a1
lw a3, 0(ca0)                # tohost lo
lw a3, 4(ca0)                # tohost hi
addi a2, zero, 0x42
sw a2, 0(ca0)                # payload 'B'
li a2, 0x01010000
sw a2, 4(ca0)                # device 1, cmd 1: putchar, port reset
lw a3, 0(ca0)

# FENCE, one per arm of its match on the low bits (RW) of pred, succ; FENCE.TSO
# (rw, rw), (none, none) and an unsupported set; FENCE.I; reserved FENCE and
# FENCE.I encodings (no-ops); SFENCE.VMA in M (no-op); WFI in M with
# mtime >= mtimecmp, then with mtimecmp (CLINT) ahead: mtime, mcycle := mtimecmp.
fence                     # iorw, iorw
fence rw, rw
fence r, rw
fence r, r
fence rw, w
fence w, w
fence w, rw
fence rw, r
fence r, w
fence w, r
fence io, io              # succ without r/w
fence o, rw               # pred without r/w
fence.tso
.word 0x8000000f          # fence.tso with no sets
.word 0x8220000f          # fence.tso r, r: unsupported, no-op
fence.i
.word 0x1ff0000f          # FENCE_RESERVED: fm = 1
.word 0x0ff0008f          # FENCE_RESERVED: rd = ra
.word 0x0010100f          # FENCEI_RESERVED: imm = 1
.word 0x0000908f          # FENCEI_RESERVED: rs = ra, rd = ra
sfence.vma
sfence.vma a0, a1
wfi                       # mtime = mtimecmp = 0: no change
cspecialr ca0, mtdc
li a1, 0x02004000         # CLINT mtimecmp
csetaddr ca0, ca0, a1
addi a2, zero, 100
sw a2, 0(ca0)             # mtimecmp = 100
sw zero, 4(ca0)
wfi                       # mtime, mcycle = 100
csrrs a3, mcycle, zero

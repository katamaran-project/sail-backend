# CMove, CClearTag, CGetAddr, CGetType, CGetHigh, CSetHigh, CSEQX, CSub,
# CTestSubset: on the root, a derived bounded capability, an untagged copy,
# null, garbage high bits (CSetHigh 0xffffffff).
# Integers in t0-t2, capabilities in ca0-ca3, cs0, cs1.
cspecialr ca0, mtdc
li t0, 0x80002000
csetaddr ca0, ca0, t0
addi t1, zero, 64
csetbounds ca1, ca0, t1   # [0x80002000, +64)
cincaddrimm ca1, ca1, 8
cmove ca2, ca1
cmove ca3, cnull
cgetaddr t2, ca1
cgetaddr t2, cnull
cgettype t2, ca1
cgettype t2, cnull
cgethigh t2, ca0          # root
cgethigh t2, ca1
cgethigh t2, cnull
csethigh cs0, ca1, t2     # high of null: untagged
cgethigh t2, ca1
csethigh cs0, ca1, t2     # same high bits: untagged copy of ca1
li t1, 0xffffffff
csethigh cs1, ca1, t1     # garbage high bits
cgethigh t2, cs1
cgetbase t2, cs1
ccleartag ca3, ca1
ccleartag ca3, ca3        # already untagged
ccleartag ca3, cnull
ccleartag ca3, ca1
cseqx t2, ca1, ca2        # equal: 1
cseqx t2, ca1, ca3        # tag differs: 0
cseqx t2, cs0, ca3        # untagged, same bits: 1
cseqx t2, ca1, ca0        # different: 0
cseqx t2, cnull, cnull
csub t2, ca1, ca0         # 8
csub t2, ca0, ca1         # -8
csub t2, ca1, cnull
ctestsubset t2, ca0, ca1  # subset: 1
ctestsubset t2, ca1, ca0  # not: 0
ctestsubset t2, ca1, ca3  # tags differ: 0
ctestsubset t2, cnull, ca3  # both untagged, ca3 outside null's bounds: 0
ctestsubset t2, cnull, cnull
addi t1, zero, 0x7f3      # drop SD, LM
candperm ca3, ca1, t1
ctestsubset t2, ca3, ca1  # more permissions: 0
ctestsubset t2, ca1, ca3  # 1

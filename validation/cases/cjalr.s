# CJALR: call through an unsealed capability into cra (sentry link),
# return through the sentry, tail call into ct1 (unsealed link), jump with offset.
cla ct0, func
cjalr cra, ct0            # call
cla ct0, tail
cjalr ct1, ct0            # link unsealed
addi a2, zero, 99         # skipped
func:
addi a0, a0, 1
cjalr zero, 0(ra)         # return (backward sentry)
tail:
cla ct0, base
cjalr zero, 8(ct0)        # to base + 8
base:
addi a2, zero, 98         # skipped
addi a2, zero, 97         # skipped
addi a3, zero, 3

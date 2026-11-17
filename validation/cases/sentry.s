# Sentries by CSeal (otypes 1-3 by the sealing root), CJALR through them:
# call into cra through an inheriting (1), disabling (2), enabling (3) forward
# sentry (MIE 0, 0, 1); tail call through an inheriting sentry into cnull and
# into ct2 (unsealed link); return through the backward sentry (otype 4 from a
# call made with MIE 0: MIE := 0). CGetType of the sentries.
# Capabilities in ct0, ct1, cra, ct2, cs0 (sealing root), cs1 (authority).
cspecialr cs0, mscratchc
addi t0, zero, 1
csetaddr cs1, cs0, t0
cla ct0, func
cseal ct0, ct0, cs1       # inheriting sentry
cgettype a0, ct0
cjalr cra, ct0            # call: allowed (forward sentry)
cgettype a0, cra          # backward sentry, MIE 0
addi t0, zero, 2
csetaddr cs1, cs0, t0
cla ct0, func
cseal ct0, ct0, cs1       # interrupt-disabling sentry
cjalr cra, ct0
addi t0, zero, 3
csetaddr cs1, cs0, t0
cla ct0, func
cseal ct0, ct0, cs1       # interrupt-enabling sentry
cjalr cra, ct0            # MIE := 1
csrrs a1, mstatus, zero
addi t0, zero, 1
csetaddr cs1, cs0, t0
cla ct0, tail1
cseal ct0, ct0, cs1
cjalr cnull, ct0          # tail call through an inheriting sentry
addi a2, zero, 99         # skipped
tail1:
cla ct0, tail2
cseal ct0, ct0, cs1
cjalr ct2, ct0            # outlined call, unsealed link in ct2
addi a2, zero, 98         # skipped
func:
addi a3, a3, 1
csrrs a1, mstatus, zero
cjalr cnull, 0(cra)       # return: backward sentry
tail2:
cgettype a0, ct2
cgettag a0, ct2
csrrs a1, mstatus, zero

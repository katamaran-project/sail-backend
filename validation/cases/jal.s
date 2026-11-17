# JAL (CJAL in CHERIoT): link into cra (a sentry), into t0 (unsealed), into x0 (J);
# return through the sentry with CJALR (cret).
jal ra, func              # cra = return sentry
jal t0, skip              # ct0 = unsealed link capability
addi a2, zero, 99         # skipped
skip:
j over
addi a2, zero, 98         # skipped
over:
addi a1, zero, 2
j end
func:
addi a0, zero, 7
cjalr zero, 0(ra)         # return
end:
addi a3, zero, 3

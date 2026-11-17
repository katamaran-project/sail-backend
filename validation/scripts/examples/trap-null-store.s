# A store through the null capability: CHERI tag violation, trap to MTCC.
# Never reaches tohost: run with --inst-limit.
addi a1, zero, 5
sw   a1, 0(ca0)           # c10 is null at reset

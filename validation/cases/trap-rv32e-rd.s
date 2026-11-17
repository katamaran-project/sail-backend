# RV32E: x16 as destination: `Error_not_rv32e_register`, caught by
# `try_execute`: traps illegal (mcause 2).
addi a1, zero, 5
addi x16, zero, 1
addi a2, zero, 99         # not reached

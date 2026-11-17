# RV32E: x31 as source: `Error_not_rv32e_register`, caught by
# `try_execute`: traps illegal (mcause 2).
addi a1, zero, 5
add a2, a1, x31
addi a3, zero, 99         # not reached

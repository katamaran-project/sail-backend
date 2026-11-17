# M extension: MUL, high products, DIV/REM incl. division by zero and overflow.
li a0, 0x80000000         # INT_MIN
addi a1, zero, -1
addi a2, zero, 7
mul a3, a0, a2            # low word of INT_MIN * 7
mulh a3, a0, a2
mulhu a3, a0, a2
mulhsu a3, a1, a2         # -1 * 7 (signed x unsigned): high = -1
div a3, a0, a1            # INT_MIN / -1 = INT_MIN (overflow)
rem a3, a0, a1            # 0
div a3, a2, zero          # by zero: -1
divu a3, a2, zero         # by zero: 0xffffffff
rem a3, a2, zero          # by zero: 7
remu a3, a2, a1           # 7 mod 0xffffffff = 7
addi a2, zero, -7
div a3, a2, zero
div a3, a2, a0
rem a3, a2, a1

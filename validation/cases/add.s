# ADD and SUB: wrap-around at 2^32, negative results, x0 sources.
li a0, 0x7fffffff
addi a1, zero, 1
add a2, a0, a1            # 0x80000000 (signed overflow)
li a3, 0xffffffff
add a2, a3, a1            # 0 (carry out)
sub a3, zero, a1          # -1
sub a1, a1, a0            # 1 - 0x7fffffff
add a0, zero, zero

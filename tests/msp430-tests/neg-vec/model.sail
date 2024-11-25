default Order dec
$include <prelude.sail>
$include <string.sail>
$include <smt.sail>
$include <elf.sail>


val zero_extend : bits(1) -> bits(32)
function zero_extend(v) = sail_zero_extend(v, 32)



val neg_vec: bits(32) -> bits(32)
function clause neg_vec(a) = {
  let r = 0b0 in 
  sub_bits(zero_extend(r), a)
}

overload neg = {neg_vec}


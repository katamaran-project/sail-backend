default Order dec

$include <prelude.sail>

/*
   Adding a computed integer, which may be negative, to a bitvector (add_bits_int).
   Shaped like getCapBoundsBits in CHERIoT's cheri_cap_common.sail, where
   the correction c_b is 0 or -1.

   a_hi is annotated with int: without it, its type is int(if high then 1 else 0),
   an `if` inside a numeric type, which is a separate gap (inventory pile P11)
   and which getCapBoundsBits itself also has.
*/

val correct_base : (bits(32), bool) -> bits(32)
function correct_base(a, high) = {
  let a_hi : int = if high then 1 else 0;
  let c_b = 0 - a_hi;
  a + c_b
}

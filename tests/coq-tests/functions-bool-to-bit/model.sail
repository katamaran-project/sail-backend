default Order dec

$include <prelude.sail>


enum my_bit = { zero, one }

val bool_to_bit : bool -> my_bit
function bool_to_bit(x) = {
  if x then one else zero
}
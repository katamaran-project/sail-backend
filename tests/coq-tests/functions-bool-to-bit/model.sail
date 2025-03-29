default Order dec

$include <prelude.sail>


enum my_bit = { my_zero, my_one }

val bool_to_bit : bool -> my_bit
function bool_to_bit(x) = {
  if x then my_one else my_zero
}
default Order dec

$include <prelude.sail>


enum my_bit = { zero, one }

val flip : my_bit -> my_bit
function flip(x) = {
  match x {
    zero => one,
    one  => zero
  }
}
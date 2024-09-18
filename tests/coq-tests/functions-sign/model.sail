default Order dec

$include <prelude.sail>


enum sign = { positive, zero, negative }

val sign : int -> sign
function sign(x) = {
  if x < 0 then { negative }
  else if x > 0 then { positive }
  else { zero }
}
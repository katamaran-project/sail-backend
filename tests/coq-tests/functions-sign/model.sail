default Order dec

$include <prelude.sail>


enum sign = { sign_positive, sign_zero, sign_negative }

val sign : int -> sign
function sign(x) = {
  if x < 0 then { sign_negative }
  else if x > 0 then { sign_positive }
  else { sign_zero }
}

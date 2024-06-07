default Order dec

$include <prelude.sail>


val sign : int -> int
function sign(x) = {
  if x > 0 then { 1 }
  else { -1 }
}
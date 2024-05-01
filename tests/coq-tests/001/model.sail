default Order dec

$include <prelude.sail>


val double : int -> int
function double(x) = {
  if x > 0 then { 1 }
  else { -1 }
}
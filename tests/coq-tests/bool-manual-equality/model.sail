default Order dec

$include <prelude.sail>


val eq : (bool, bool) -> bool
function eq(x, y) = {
  if x then { y }
  else if y then { false }
  else { true }
}

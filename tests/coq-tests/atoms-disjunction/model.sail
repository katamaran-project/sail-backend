default Order dec

$include <prelude.sail>


val disj : (bool, bool) -> bool
function disj(x, y) = {
  x | y
}

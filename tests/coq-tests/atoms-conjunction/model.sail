default Order dec

$include <prelude.sail>


val conj : (bool, bool) -> bool
function conj(x, y) = {
  x & y
}

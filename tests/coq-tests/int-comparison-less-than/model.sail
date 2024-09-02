default Order dec

$include <prelude.sail>


val lt : (int, int) -> bool
function lt(x, y) = {
  x < y
}

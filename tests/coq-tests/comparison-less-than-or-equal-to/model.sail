default Order dec

$include <prelude.sail>


val foo : (int, int) -> bool
function foo(x, y) = {
  x <= y
}

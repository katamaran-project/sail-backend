default Order dec

$include <prelude.sail>


val foo : (bool, bool) -> bool
function foo(x, y) = {
  x == y
}
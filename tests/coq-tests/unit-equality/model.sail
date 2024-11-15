default Order dec

$include <prelude.sail>


val foo : (unit, unit) -> bool
function foo(x, y) = {
  x == y
}
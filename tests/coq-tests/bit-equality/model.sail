default Order dec

$include <prelude.sail>


val foo : (bit, bit) -> bool
function foo(x, y) = {
  x == y
}
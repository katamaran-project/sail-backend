default Order dec

$include <prelude.sail>


val foo : (int, int) -> int
function foo(x, y) = {
  x - y
}
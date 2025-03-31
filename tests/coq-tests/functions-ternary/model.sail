default Order dec

$include <prelude.sail>


val foo : (int, int, int) -> int
function foo(x, y, z) = {
  x
}


val bar : int -> int
function bar(x) = foo(x, x, x)
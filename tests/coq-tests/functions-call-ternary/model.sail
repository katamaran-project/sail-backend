default Order dec

$include <prelude.sail>


val foo : (int, int, int) -> int
function foo(x, y, z) = {
  x
}


val bar : unit -> int
function bar() = {
  foo(1, 2, 3)
}
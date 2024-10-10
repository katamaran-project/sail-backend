default Order dec

$include <prelude.sail>


val foo : int -> int
function foo(x) = {
  x
}


val bar : unit -> int
function bar() = {
  foo(1)
}
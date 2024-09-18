default Order dec

$include <prelude.sail>


val foo : int -> int
function foo(x) = {
  x
}


val bar : int -> int
function bar(x) = {
  foo(x)
}
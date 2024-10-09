default Order dec

$include <prelude.sail>


val foo : int -> bool
function foo(x) = {
  true
}

val bar : unit -> bool
function bar() = {
  foo(1+2)
}
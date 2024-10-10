default Order dec

$include <prelude.sail>


val foo : unit -> int
function foo() = {
  1
}


val bar : unit -> int
function bar() = {
  foo()
}
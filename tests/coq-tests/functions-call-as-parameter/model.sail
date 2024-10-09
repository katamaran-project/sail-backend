default Order dec

$include <prelude.sail>


val foo : unit -> int
function foo() = {
  0
}


val bar : int -> int
function bar(x) = {
  x
}


val baz : unit -> int
function baz() = {
  bar(foo())
}
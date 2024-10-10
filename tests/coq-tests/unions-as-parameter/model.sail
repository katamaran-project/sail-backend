default Order dec

$include <prelude.sail>


union instruction = {
  Pop : unit
}


val foo : instruction -> unit
function foo(x) = {
  ()
}


val bar : unit -> unit
function bar() = {
  foo(Pop ())
}
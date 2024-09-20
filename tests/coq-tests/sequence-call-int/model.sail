default Order dec

$include <prelude.sail>


val bar : int -> unit
function bar(x) = {
  ()
}

val foo : unit -> int
function foo() = {
  bar(1); 2
}

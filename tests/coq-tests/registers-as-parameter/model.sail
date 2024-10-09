default Order dec

$include <prelude.sail>


register r1 : int


val foo : int -> unit
function foo x = {
  ()
}


val bar : unit -> unit
function bar () = {
  foo(r1)
}

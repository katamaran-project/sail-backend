default Order dec

$include <prelude.sail>


union instruction = {
  Pop : unit
}


val foo : unit -> instruction
function foo () = {
  Pop ()
}
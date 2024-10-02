default Order dec

$include <prelude.sail>


union instruction = {
  Pop : (int, int)
}


val foo : unit -> instruction
function foo () = {
  Pop (2, 4)
}
default Order dec

$include <prelude.sail>


union instruction = {
  Pop : int
}


val foo : unit -> instruction
function foo () = {
  Pop 5
}
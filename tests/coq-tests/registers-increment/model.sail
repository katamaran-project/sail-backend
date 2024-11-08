default Order dec

$include <prelude.sail>


register r1 : int = 0


val foo : unit -> unit
function foo () = {
  r1 = r1 + 1;
}

default Order dec

$include <prelude.sail>

register r1 : int = 0


val foo : unit -> int
function foo() = {
  r1 = r1 + 1;
  r1
}

default Order dec

$include <prelude.sail>

register x : bits(12) = 12^0

val foo : bits(4) -> unit
function foo(y) = {
  x[3..0] = y
}
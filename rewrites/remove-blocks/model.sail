default Order dec

$include <prelude.sail>


val neg : bool -> bool
function neg(x) = {
  x != true
}
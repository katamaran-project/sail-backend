default Order dec

$include <prelude.sail>

/*
  Parameter patterns that can fail to match. Sail's own rewrites already turn these
  into a match with a failure branch before the translation sees them: the
  constructor pattern through merge#var, the literal through p0#.
*/

union shape = { Circle : bits(8), Square : bits(16) }

val radius : shape -> bits(8)
function radius (Circle(r)) = r

val when_set : (bits(1), bits(8)) -> bits(8)
function when_set (0b1, x) = x

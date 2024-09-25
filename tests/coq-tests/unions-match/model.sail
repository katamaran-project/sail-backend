default Order dec

$include <prelude.sail>


union MyUnion = {
  Nullary : unit,
  Unary   : int,
  Binary  : (int, int),
  Ternary : (int, int, int)
}

val arity : MyUnion -> int
function arity(u) = match u {
  Nullary()        => 0,
  Unary(x)         => 1,
  Binary(x, y)     => 2,
  Ternary(x, y, z) => 3
}

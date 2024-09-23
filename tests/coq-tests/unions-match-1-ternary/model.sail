default Order dec

$include <prelude.sail>


union MyUnion = {
  Push : (int, int, int)
}

val foo : MyUnion -> int
function foo(u) = match u {
  Push(x, y, z) => x + y + z
}

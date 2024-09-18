default Order dec

$include <prelude.sail>


union MyUnion = {
  Push : int
}

val foo : MyUnion -> int
function foo(u) = match u {
  Push(x) => x
}

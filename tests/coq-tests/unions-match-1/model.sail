default Order dec

$include <prelude.sail>


union MyUnion = {
  Push : unit
}

val foo : MyUnion -> int
function foo(u) = match u {
  Push() => 1
}

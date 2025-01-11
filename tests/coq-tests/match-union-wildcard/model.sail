default Order dec

$include <prelude.sail>


union MyUnion = {
  Push  : unit,
  Pop   : unit,
  Clear : unit,
  Halt  : unit,
}

val foo : MyUnion -> int
function foo(u) = match u {
  Push() => 1,
  Pop()  => 2,
  _      => 3
}

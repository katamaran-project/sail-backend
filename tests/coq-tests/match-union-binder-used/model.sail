default Order dec

$include <prelude.sail>


union MyUnion = {
  Push  : unit,
  Pop   : unit,
  Clear : unit,
  Halt  : unit,
}

val bar : MyUnion -> int
function bar _ = 0

val foo : MyUnion -> int
function foo(u) = match u {
  Push() => 1,
  Pop()  => 2,
  x      => bar(x)
}

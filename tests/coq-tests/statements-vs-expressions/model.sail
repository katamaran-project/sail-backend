default Order dec

$include <prelude.sail>

register r1 : int
register r2 : int

val foo : (int) -> int
function foo (x) = x

val bar : unit -> int
function bar () =
  foo(r1) + foo(r2)

default Order dec

$include <prelude.sail>

union exception = {
  Error_bad : unit
}

/*
   f's try body cannot throw: f is its body, pure_add keeps its type.
   unreached throws, but no try reaches it: its throw fails and it keeps its type.
*/
val pure_add : int -> int
function pure_add(x) = x + 1

val f : int -> int
function f(x) = {
  try pure_add(x) catch {
    Error_bad() => 0
  }
}

val unreached : int -> int
function unreached(x) = {
  if x > 15 then throw(Error_bad());
  x
}

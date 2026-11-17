default Order dec

$include <prelude.sail>

union exception = {
  Error_a : unit,
  Error_b : int,
  Error_c : unit
}

/*
   No try catches Error_b: its throw fails, as does top's rethrow.
   middle's try catches Error_c and rethrows the rest: Error_a is raised on to top, Error_b fails.
*/
val leaf : int -> int
function leaf(x) = {
  if x == 0 then throw(Error_a());
  if x == 1 then throw(Error_b(x));
  if x == 2 then throw(Error_c());
  x
}

val middle : int -> int
function middle(x) = {
  try leaf(x) catch {
    Error_c() => 1,
    e => throw(e)
  }
}

val top : int -> int
function top(x) = {
  try middle(x) catch {
    Error_a() => 0,
    e => throw(e)
  }
}

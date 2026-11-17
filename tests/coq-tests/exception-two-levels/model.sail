default Order dec

$include <prelude.sail>

union exception = {
  Error_bad : unit
}

/*
   The exception travels through two calls (level2, level1) to top's try.
   Both raise: each call in level1 is followed by a match that returns inr e on an exception,
   also inside a loop helper and in an assignment.
*/
val level2 : int -> int
function level2(x) = {
  if x > 15 then throw(Error_bad());
  x + 1
}

val level1 : int -> int
function level1(x) = {
  let y = level2(x);
  var acc : int = 0;
  foreach (i from 0 to 3) {
    acc = acc + level2(i)
  };
  acc = level2(acc);
  y + acc
}

val top : int -> int
function top(x) = {
  try level1(x) catch {
    Error_bad() => 0
  }
}

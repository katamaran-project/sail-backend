default Order dec

$include <prelude.sail>

union exception = {
  Error_bad : unit
}

/*
   foo's try catches Error_bad, so check raises it as a value:
   check returns ty.sum ty.int (ty.union Uexception), and foo matches on its result.
*/
val check : int -> int
function check(x) = {
  if x > 15 then throw(Error_bad());
  x + 1
}

val foo : int -> int
function foo(x) = {
  try check(x) catch {
    Error_bad() => 0
  }
}

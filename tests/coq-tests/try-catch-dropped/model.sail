default Order dec

$include <prelude.sail>

union exception = {
  Error_bad : unit
}

val check : int -> int
function check(x) = {
  if x > 15 then throw(Error_bad());
  x + 1
}

/*
   try { e } catch { ... } is translated as e: the handler is dropped,
   so fun_foo is a call to check, and check's throw is stm_fail.
*/
val foo : int -> int
function foo(x) = {
  try check(x) catch {
    Error_bad() => 0
  }
}

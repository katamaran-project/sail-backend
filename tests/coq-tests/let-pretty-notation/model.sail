val foo : unit -> int
function foo() = {
  let x = 5 in x
}


/*
  We don't want to include prelude.sail needlessly.
  However, in order to prevent the let-structure from being optimized away in bar,
  we need to make use of x, y and z somehow.
  Simplest way is to use them to call a dummy function.
*/
val dummy : (int, int, int) -> int
function dummy(x, y, z) = x


val bar : unit -> int
function bar() = {
  let x = 1 in
  let y = 2 in
  let z = 3 in
  dummy(x, y, z)
}

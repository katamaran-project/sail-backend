default Order dec

$include <prelude.sail>


val my_map : int <-> bool
mapping my_map = {
  0 <-> false,
  1 <-> true
}

val foo : int -> bool
function foo(n) = {
  my_map(n)
}

val bar : bool -> int
function bar(b) = {
  my_map(b)
}
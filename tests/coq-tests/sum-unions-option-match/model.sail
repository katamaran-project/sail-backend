default Order dec

$include <prelude.sail>


val get_or_zero : option(int) -> int
function get_or_zero(x) = match x {
  Some(n) => n,
  None()  => 0
}

val is_something : option(bits(4)) -> bool
function is_something(x) = match x {
  None() => false,
  _      => true
}

val increment : option(int) -> option(int)
function increment(x) = match x {
  Some(n) => Some(n + 1),
  y       => y
}

default Order dec

$include <prelude.sail>


val select : (option(int), bool) -> int
function select(x, b) = match (x, b) {
  (Some(n), true) => n,
  (Some(_), false) => 1,
  (None(), _) => 0
}

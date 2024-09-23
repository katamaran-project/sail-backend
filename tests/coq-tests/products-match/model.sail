default Order dec

$include <prelude.sail>


val foo : ((int, int), int) -> int
function foo(pair, n) = match pair {
  (x, y) => x + y + n
}

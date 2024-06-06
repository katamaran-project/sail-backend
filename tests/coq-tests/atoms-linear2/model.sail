default Order dec

$include <prelude.sail>


val foo : int -> int
function foo(x) = {
  3 * x - x + 5
}
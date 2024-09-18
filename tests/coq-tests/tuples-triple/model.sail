default Order dec

$include <prelude.sail>


val foo : unit -> (int, int, int)
function foo() = {
  (1, 2, 3)
}
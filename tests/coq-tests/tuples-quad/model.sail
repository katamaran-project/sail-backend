default Order dec

$include <prelude.sail>


val foo : unit -> (int, bool, int, bool)
function foo() = {
  (1, true, 3, false)
}
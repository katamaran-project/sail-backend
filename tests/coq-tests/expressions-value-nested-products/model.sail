default Order dec

$include <prelude.sail>


val foo : unit -> ((int, string), (bool, unit))
function foo() = {
  ((1, "abc"), (false, ()))
}

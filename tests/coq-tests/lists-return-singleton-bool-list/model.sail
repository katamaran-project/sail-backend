default Order dec

$include <prelude.sail>


val foo : unit -> list(bool)
function foo() = {
  [| true |]
}
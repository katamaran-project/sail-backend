default Order dec

$include <prelude.sail>


val foo : unit -> int
function foo() = {
  let x = 1
  in
  let y = 2
  in
  x + y
}

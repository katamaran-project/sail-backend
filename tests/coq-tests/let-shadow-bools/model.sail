default Order dec

$include <prelude.sail>


val foo : unit -> bool
function foo() = {
  let x = true
  in
  let y = true
  in
  let x = false
  in
  x
}

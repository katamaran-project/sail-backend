default Order dec

$include <prelude.sail>


val foo : unit -> bool
function foo() = {
  let x = true
  in
  let y = false
  in  
  x | y
}

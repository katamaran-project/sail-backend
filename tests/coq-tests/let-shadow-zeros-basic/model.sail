default Order dec

$include <prelude.sail>


val foo : unit -> bits(10)
function foo() = {
  let x = 1 in
  let x = 10 in
  sail_zeros(x)
}

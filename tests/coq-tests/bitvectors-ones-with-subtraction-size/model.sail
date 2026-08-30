default Order dec

$include <prelude.sail>


val foo : unit -> bits(7)
function foo() = {
  let x = 8 in
  sail_ones(x - 1)
}

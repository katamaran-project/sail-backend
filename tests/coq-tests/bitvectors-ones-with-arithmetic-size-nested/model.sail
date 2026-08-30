default Order dec

$include <prelude.sail>


val foo : unit -> bits(12)
function foo() = {
  let x = 2 in
  let x = x + x in
  sail_ones(x * 3)
}

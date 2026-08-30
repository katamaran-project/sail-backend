default Order dec

$include <prelude.sail>


val foo : unit -> bits(17)
function foo() = {
  let x = 8 in
  sail_ones(x * 2 + 1)
}

default Order dec

$include <prelude.sail>


val foo : unit -> bits(17)
function foo() = {
  sail_ones(8 * 2 + 1)
}

default Order dec

$include <prelude.sail>


val foo : unit -> bits(11)
function foo() = {
  let x = 1 in
  let y = x + 1 in
  let x = 10 in
  let y = x + 1 in
  sail_ones(y)
}

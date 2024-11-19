default Order dec

$include <prelude.sail>


let size = 32

type word = bits(32)



val foo : unit -> word
function foo() = {
  sail_zeros(size)
}

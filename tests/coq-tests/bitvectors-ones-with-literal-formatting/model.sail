default Order dec

$include <prelude.sail>


type word = bits(32)


val foo : unit -> word
function foo() = {
  sail_ones(32)
}

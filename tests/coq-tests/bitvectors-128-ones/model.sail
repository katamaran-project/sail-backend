default Order dec

$include <prelude.sail>


type word = bits(128)


val foo : unit -> word
function foo() = {
  sail_ones(128)
}

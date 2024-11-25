default Order dec

$include <prelude.sail>


type size : Int = 32
type word = bits(size)

val foo : unit -> word
function foo() = {
  sail_zeros(sizeof(size))
}

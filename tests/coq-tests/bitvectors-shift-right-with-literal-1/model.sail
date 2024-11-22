default Order dec

$include <prelude.sail>


type word = bits(32)


val foo : (word) -> word
function foo(x) = {
  sail_shiftright(x, 1)
}

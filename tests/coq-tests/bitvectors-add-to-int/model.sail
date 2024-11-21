default Order dec

$include <prelude.sail>


type word = bits(32)


val foo : (word, int) -> word
function foo(x, y) = {
  x + y
}

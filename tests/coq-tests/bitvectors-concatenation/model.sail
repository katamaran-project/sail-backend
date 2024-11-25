default Order dec

$include <prelude.sail>


type word = bits(32)


val foo : (word, word) -> bits(64)
function foo(x, y) = {
  x @ y
}

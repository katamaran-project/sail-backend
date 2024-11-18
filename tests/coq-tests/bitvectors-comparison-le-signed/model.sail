default Order dec

$include <prelude.sail>


type word = bits(32)


val foo : (word, word) -> bool
function foo(x, y) = {
  signed(x) <= signed(y)
}

default Order dec

$include <prelude.sail>


type word = bits(32)


val foo : (word) -> bits(35)
function foo(x) = {
  x @ 0b000
}

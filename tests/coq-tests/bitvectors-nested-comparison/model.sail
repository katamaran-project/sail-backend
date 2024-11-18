default Order dec

$include <prelude.sail>


type word = bits(32)


val bar : bool -> bool
function bar(x, y) = {
  x & y
}


val foo : (word, word) -> bool
function foo(x, y) = {
  if (unsigned(x) < unsigned(y))
  then true
  else bar(signed(y) > signed(x))
}

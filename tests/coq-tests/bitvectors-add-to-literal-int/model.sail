default Order dec

$include <prelude.sail>


val foo : bits(32) -> bits(32)
function foo(x) = {
  x + 1
}

val bar : bits(16) -> bits(16)
function bar(x) = {
  x + 2
}

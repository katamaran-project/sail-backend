default Order dec

$include <prelude.sail>


val foo : bits(32) -> bits(32)
function foo(x) = {
  x + 1
}

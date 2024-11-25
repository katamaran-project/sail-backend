default Order dec

$include <prelude.sail>


let k = 123

val foo : bits(32) -> bits(32)
function foo(x) = {
  x + k
}

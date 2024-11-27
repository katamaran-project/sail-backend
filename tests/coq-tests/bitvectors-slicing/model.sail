default Order dec

$include <prelude.sail>


val foo : bits(12) -> bits(8)
function foo(x) = {
  x[7..0]
}
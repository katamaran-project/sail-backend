default Order dec

$include <prelude.sail>


val foo : bits(16) -> bits(16)
function foo(x) = {
  not_vec(x)
}
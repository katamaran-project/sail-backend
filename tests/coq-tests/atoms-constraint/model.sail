default Order dec

$include <prelude.sail>


val foo : forall 'k, 'k > 0. int('k) -> int('k)
function foo(x) = x
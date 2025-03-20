default Order dec

$include <prelude.sail>


val foo : forall 'b. bool('b) -> bool('b)
function foo(x) = x

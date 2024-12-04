default Order dec

$include <prelude.sail>


val foo : forall 'n. int('n) -> int(2 * 'n)
function foo(x) = 2 * x


val bar : int -> int
function bar(x) = foo(x)

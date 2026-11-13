default Order dec



val foo : forall 'n. int('n) -> int('n)
function foo(x) = x


val bar : int -> int
function bar(x) = foo(x)

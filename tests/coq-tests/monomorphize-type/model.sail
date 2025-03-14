val foo : forall 'a 'n. vector('a, 'n) -> vector('a, 'n)
function foo(xs) = {
  xs
}

val bar : vector(8, int) -> vector(8, int)
function bar(x) = foo(x)


val baz : vector(16, bit) -> vector(16, bit)
function baz(x) = foo(x)


val qux : vector(32, bool) -> vector(32, bool)
function qux(x) = foo(x)

default Order dec

$include <prelude.sail>


val foo : forall 'n. bits('n) -> bits(2 * 'n)
function foo(x) = {
  x @ x
}


val bar : bits(5) -> bits(10)
function bar(x) = {
  foo(x)
}


val baz : bits(8) -> bits(16)
function baz(x) = {
  foo(x)
}

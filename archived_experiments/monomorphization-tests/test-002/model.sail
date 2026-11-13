default Order dec

$include <prelude.sail>


val foo : forall 'n. bits('n) -> bits('n)
function foo(x) = {
  x
}


val bar : bits(5) -> bits(5)
function bar(x) = {
  foo(x)
}


val baz : bits(8) -> bits(8)
function baz(x) = {
  foo(x)
}

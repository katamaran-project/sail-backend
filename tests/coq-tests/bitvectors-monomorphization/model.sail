default Order dec

$include <prelude.sail>


val foo : forall 'n. bits('n) -> int
function foo(x) = {
  0
}


val bar : bits(5) -> int
function bar(x) = {
  foo(x)
}


val baz : bits(8) -> int
function baz(x) = {
  foo(x)
}


val qux : bits(8) -> int
function qux(x) = {
  foo(x)
}

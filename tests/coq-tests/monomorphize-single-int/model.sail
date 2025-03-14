default Order dec


val foo : forall 'n. bitvector('n) -> bitvector('n)
function foo(x) = {
  x
}


val bar : bitvector(8) -> bitvector(8)
function bar(x) = foo(x)


val baz : bitvector(16) -> bitvector(16)
function baz(x) = foo(x)


val qux : bitvector(32) -> bitvector(32)
function qux(x) = foo(x)
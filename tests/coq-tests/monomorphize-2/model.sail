default Order dec


val foo : forall 'n 'm. (bitvector('n), bitvector('m)) -> bitvector(4)
function foo(xs, ys) = {
  0x0
}


val bar : unit -> bitvector(4)
function bar() = foo(0b0, 0b00)


val baz : unit -> bitvector(4)
function baz(x) = foo(0b0000, 0b0)


val qux : unit -> bitvector(4)
function qux(x) = foo(0x12, 0x23)
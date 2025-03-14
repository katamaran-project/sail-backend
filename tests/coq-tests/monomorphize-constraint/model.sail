default Order dec

$include <prelude.sail>


val foo : forall 'n 'm, 'n > 0 & 'm > 0. (bitvector('n), bitvector('m), int('n + 'm)) -> bitvector('n + 'm)
function foo(xs, ys, n) = {
  sail_zero_extend(0b0, n)
}


val bar : unit -> bitvector(3)
function bar() = foo(0b0, 0b00, 3)


val baz : unit -> bitvector(5)
function baz(x) = foo(0b0000, 0b0, 5)


val qux : unit -> bitvector(16)
function qux(x) = foo(0x12, 0x23, 16)
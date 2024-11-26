default Order dec

$include <prelude.sail>


val bar : unit -> bits(4)
function bar() = 0b0000


val foo : unit -> bits(8)
function foo(x) = {
  bar() @ bar()
}

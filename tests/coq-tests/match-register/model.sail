default Order dec

$include <prelude.sail>


enum MyEnum = { x, y, z }

register r1 : MyEnum = x

val foo : unit -> int
function foo() = match r1 {
  x => 1,
  y => 2,
  z => 3
}

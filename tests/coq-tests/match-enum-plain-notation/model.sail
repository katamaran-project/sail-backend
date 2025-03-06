default Order dec

$include <prelude.sail>


enum MyEnum = { x, y, z }


val foo : MyEnum -> int
function foo(e) = match e {
  x => 1,
  y => 2,
  z => 3
}

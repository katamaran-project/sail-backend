default Order dec

$include <prelude.sail>


enum MyEnum = { x, y }


val foo : MyEnum -> int
function foo(e) = match e {
  x => 1,
  y => 2
}

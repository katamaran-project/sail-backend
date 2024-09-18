default Order dec

$include <prelude.sail>


enum MyEnum = { x }


val foo : MyEnum -> int
function foo(e) = match e {
  x => 1
}

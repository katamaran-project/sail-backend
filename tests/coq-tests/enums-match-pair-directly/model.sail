default Order dec

$include <prelude.sail>


enum MyEnum = { x }


val foo : (MyEnum, MyEnum) -> int
function foo(e1, e2) = {
  match (e1, e2) {
    (x, x) => 1
  }
}

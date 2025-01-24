default Order dec

$include <prelude.sail>


enum MyEnum = { x, y }


val foo : (MyEnum, MyEnum) -> int
function foo(pair) = {
  match pair {
    (x, x) => 1,
    (x, y) => 2,
    (y, x) => 3,
    (y, y) => 4
  }
}

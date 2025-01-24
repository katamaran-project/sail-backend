default Order dec

$include <prelude.sail>


enum MyEnum = { x, y }


val foo : (MyEnum, MyEnum, MyEnum) -> int
function foo(pair) = {
  match pair {
    (x, x, x) => 1,
    (x, x, y) => 2,
    (x, y, x) => 3,
    (x, y, y) => 4,
    (y, x, x) => 5,
    (y, x, y) => 6,
    (y, y, x) => 7,
    (y, y, y) => 8,
  }
}

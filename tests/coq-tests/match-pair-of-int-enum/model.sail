default Order dec

$include <prelude.sail>


enum MyEnum = {
  Foo,
  Bar
}

val fst : (int, MyEnum) -> int
function fst(pair) = {
  match pair {
    (x, Foo) => x,
    (x, Bar) => x + 1,
  }
}

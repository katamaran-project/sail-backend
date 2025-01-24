default Order dec

$include <prelude.sail>


enum MyEnum = {
  Foo,
  Bar
}

val fst : (MyEnum, int) -> int
function fst(pair) = {
  match pair {
    (Foo, x) => x,
    (Bar, x) => x + 1,
  }
}

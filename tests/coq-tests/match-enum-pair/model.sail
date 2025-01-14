default Order dec

$include <prelude.sail>


enum MyEnum = { x }


val foo : (MyEnum, MyEnum) -> int
function foo(args) = {
  match args {
    (x, x) => 1
  }
}

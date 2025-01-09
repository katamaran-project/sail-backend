default Order dec

$include <prelude.sail>


enum MyEnum = { x }


val foo : (MyEnum, MyEnum) -> int
function foo(e1, e2) = {
  let bar = (e1, e2)
  in
  match bar {
    (x, x) => 1
  }
}

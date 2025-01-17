default Order dec

$include <prelude.sail>


struct MyStruct = {
  foo : int,
  bar : int,
  qux : bool,
}


val func : MyStruct -> int
function func(s) = {
  match s {
    struct { foo = x, bar = y, qux = z } => x
  }
}
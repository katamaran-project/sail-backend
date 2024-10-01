default Order dec

$include <prelude.sail>


struct MyStruct = {
  foo : int,
  bar : bool,
  baz : (int, int),
  qux : (int, int, int)
}

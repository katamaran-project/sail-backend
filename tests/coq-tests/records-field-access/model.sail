default Order dec

$include <prelude.sail>


struct MyStruct = {
  foo : int,
  bar : bool
}


val get_foo : MyStruct -> int
function get_foo(s) = {
  s.foo
}

default Order dec

$include <prelude.sail>


struct MyStruct = {
  x : int
}

union Foo = {
  FooX : MyStruct
}

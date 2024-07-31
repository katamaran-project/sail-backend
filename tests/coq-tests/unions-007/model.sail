default Order dec

$include <prelude.sail>


union Foo = {
  FooX : int
}

union Bar = {
  BarX : Foo
}

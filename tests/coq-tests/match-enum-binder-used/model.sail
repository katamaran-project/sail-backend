default Order dec

$include <prelude.sail>


enum MyEnum = { Foo, Bar, Baz }

val bar : MyEnum -> int
function bar _ = 0

val foo : MyEnum -> int
function foo(e) = match e {
  Foo => 1,
  x => bar(x)
}

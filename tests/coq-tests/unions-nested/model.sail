default Order dec

$include <prelude.sail>


enum MyEnum = { AAA, BBB, CCC }

union MyUnion = {
  Bar : MyEnum,
  Baz : MyEnum
}

struct MyStruct = {
  Qux : MyUnion
}

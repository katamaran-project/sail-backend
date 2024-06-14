default Order dec

$include <prelude.sail>


enum MyEnum1 = { x1, y1, z1 }

enum MyEnum2 = { x2, y2, z2 }


union MyUnion1 = {
  Add1 : int,
  Sub1 : unit,
  Mul1 : (int, int),
  Ternary1 : (int, int, int)
}

union MyUnion2 = {
  Add2 : int,
  Sub2 : unit,
  Mul2 : (int, int)
}

struct MyStruct1 = {
  foo1 : int,
  bar1 : int,
  baz1 : int,
  qux1 : int
}

struct MyStruct2 = {
  foo2 : int,
  bar2 : int,
  baz2 : int,
  qux2 : int
}
default Order dec

$include <prelude.sail>


enum MyEnum1 = { x1, y1, z1 }

enum MyEnum2 = { x2, y2, z2 }


union instruction1 = {
  Add1 : int,
  Sub1 : unit,
  Mul1 : (int, int),
  Ternary1 : (int, int, int)
}

union instruction2 = {
  Add2 : int,
  Sub2 : unit,
  Mul2 : (int, int)
}

struct Foo = {
  foo : int,
  bar : int,
  baz : int,
  qux : int
}
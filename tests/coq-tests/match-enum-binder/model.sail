default Order dec

$include <prelude.sail>


enum MyEnum = { x, y, z }


val use : MyEnum -> int
function use _ = 0


val foo : MyEnum -> int
function foo(e) = match e {
  x => use(x),
  other => use(other)
}

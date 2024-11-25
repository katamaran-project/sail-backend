default Order dec

$include <prelude.sail>


enum MyEnum = { Foo, Bar, Baz }

val my_enum_to_int : MyEnum <-> int
mapping my_enum_to_int = {
  Foo <-> 1,
  Bar <-> 2,
  Baz <-> 4
}
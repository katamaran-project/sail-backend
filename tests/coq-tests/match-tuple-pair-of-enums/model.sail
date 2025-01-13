default Order dec

$include <prelude.sail>


enum MyEnum = {
  Foo,
  Bar
}


val my_function : (MyEnum, MyEnum) -> string
function my_function(x) = {
  match x {
    (Foo, Foo) => "A",
    (Foo, x  ) => "B",
    (Bar, Foo) => "C",
    (Bar, _  ) => "D"
  }
}
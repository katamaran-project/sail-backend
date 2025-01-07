default Order dec

$include <prelude.sail>


enum Foo = {
  Foo,
  Bar
}


val my_function : (Foo, Foo) -> string
function my_function(x) = {
  match x {
    (Foo, Foo) => "A",
    (Foo, Bar) => "B",
    (Bar, Foo) => "C",
    (Bar, Bar) => "D"
  }
}
default Order dec

$include <prelude.sail>

union MyUnion = {
  Foo : unit,
  Bar : unit
}

val my_function : (MyUnion, int) -> string
function my_function(pair) = {
  match pair {
    (Foo(x), 0) => "A",
    (Foo(x), _) => "B",
    (Bar(x), 0) => "C",
    (Bar(x), _) => "D"
  }
}
union MyUnion = {
  Foo : unit,
  Bar : unit
}


val my_function : (MyUnion, MyUnion) -> string
function my_function(pair) = {
  match pair {
    (Foo(x), Foo(y)) => "A",
    (Foo(x), _     ) => "B",
    (Bar(x), _     ) => "C",
  }
}

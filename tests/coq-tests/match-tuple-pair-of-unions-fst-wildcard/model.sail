union MyUnion = {
  Foo : unit,
  Bar : unit
}


val my_function : (MyUnion, MyUnion) -> string
function my_function(pair) = {
  match pair {
    (_, Foo(y)) => "A",
    (_, Bar(y)) => "B",
  }
}

union MyUnion = {
  Foo : unit,
  Bar : unit
}


val my_function : (MyUnion, MyUnion) -> string
function my_function(a, b) = {
  let pair = (a, b) in
  match pair {
    (Foo(x), Foo(y)) => "A",
    (Foo(x), Bar(y)) => "B",
    (Bar(x), Foo(y)) => "C",
    (Bar(x), Bar(y)) => "D"
  }
}
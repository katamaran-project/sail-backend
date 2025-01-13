union MyUnion = {
  Foo : unit,
  Bar : unit
}


val my_function : (MyUnion, MyUnion) -> string
function my_function(a, b) = {
  let pair = (a, b) in
  match pair {
    (Foo(), Foo()) => "A",
    (Foo(), Bar()) => "B",
    (Bar(), Foo()) => "C",
    (Bar(), Bar()) => "D"
  }
}
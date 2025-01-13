union Foo = {
  Foo : unit,
  Bar : unit
}


val my_function : (Foo, Foo) -> string
function my_function(x) = {
  match x {
    (Foo(), Foo()) => "A",
    (Foo(), Bar()) => "B",
    (Bar(), Foo()) => "C",
    (Bar(), Bar()) => "D"
  }
}
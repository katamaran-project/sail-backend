val foo : list(int) -> bool
function foo(l) = match l {
  x => false
}

val bar : list(int) -> bool
function bar(l) = match l {
  _ => false
}

val use : list(int) -> bool
function use _ = true

val foo : list(int) -> bool
function foo(l) = match l {
  x => use(x)
}

val bar : list(int) -> bool
function bar(l) = match l {
  _ => false
}

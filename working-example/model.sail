default Order dec

$include <prelude.sail>


enum foo = A | B


val bar : foo -> bool
function bar(tralala) = {
  match tralala {
    A => true,
    B => false
  }
}


val qux : list(int) -> int
function qux(xs : list(int)) = {
  match xs {
    [||] => 1,
    h::t => 2
  }
}

val baz : (int, int) -> int
function baz(pair) = {
  match pair {
   (x, y) => x + y
  }
}
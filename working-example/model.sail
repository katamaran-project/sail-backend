default Order dec

$include <prelude.sail>


enum foo = A | B | C


val bar : foo -> int
function bar(x) = {
  match x {
    A => 0,
    B => 1,
    C => 2
  }
}


val qux : list(int) -> int
function qux(xs : list(int)) = {
  match xs {
    [||] => 1,
    h::t => 2
  }
}

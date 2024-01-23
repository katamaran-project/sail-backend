default Order dec

$include <prelude.sail>


enum foo = A | B | C

register R : foo


val bar : int -> int
function bar(x) = {
  match R {
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



// mapping clause instruction_mapping = ADD(n) <-> n @ 0b001

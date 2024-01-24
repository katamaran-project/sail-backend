default Order dec

$include <prelude.sail>


enum foo = AAAA | BBBB | CCCC

union ast = {
  Add : (int, int),
  Lit : int
}

register R1 : foo
register R2 : foo
register R3 : foo
register R4 : foo


val bar : int -> int
function bar(x) = {
  match R1 {
    AAAA => 0,
    BBBB => 1,
    CCCC => 2
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

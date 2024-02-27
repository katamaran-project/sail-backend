default Order dec

$include <prelude.sail>


union foo = {
  A : (int, int),
  B : int,
}

union bar = {
  C : foo
}


// Scattered functions
val qux : foo -> int
scattered function qux

function clause qux(A(a, b)) = { a + b }
function clause qux(B(a))    = { a }

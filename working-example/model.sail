default Order dec

$include <prelude.sail>


union plumbus = {
  A : (int, int),
  B : int,
  C : unit
}


// Scattered functions
val bar : plumbus -> int
scattered function bar

function clause bar(A(x, y)) = { 0 }
function clause bar(B(n)) = { 0 }
function clause bar(C()) = { 0 }

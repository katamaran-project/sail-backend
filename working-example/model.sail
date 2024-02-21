default Order dec

$include <prelude.sail>


union plumbus = {
  A : int,
  B : int,
}


// Scattered functions
val bar : plumbus -> int
scattered function bar

function clause bar(A(n)) = { 0 }
function clause bar(B(n)) = { 0 }

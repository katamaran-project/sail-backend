default Order dec

$include <prelude.sail>


enum foo = AAAA | BBBB | CCCC

union plumbus = {
  A : int,
  B : int,
}


mapping double_map : int <-> foo = {
  1 <-> AAAA,
  2 <-> BBBB,
  3 <-> CCCC
}


// Scattered functions
val bar : plumbus -> int
scattered function bar

function clause bar(A(n)) = { 0 }
function clause bar(B(n)) = { 0 }

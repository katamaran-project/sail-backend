default Order dec

$include <prelude.sail>

/*
   Adding an integer that is only known at run time to a bitvector (add_bits_int).
   The constant case is covered by bitvectors-add-to-literal-int.
*/

val foo : (bits(32), int) -> bits(32)
function foo(x, i) = {
  x + i
}

val bar : (bits(16), int) -> bits(16)
function bar(x, i) = {
  x + i
}

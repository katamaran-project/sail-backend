default Order dec

$include <prelude.sail>

/*
  Two parameters that are patterns: each gets its own fresh name, arg#0 and arg#1,
  and the body matches on both, the first one outermost.
*/

newtype regidx = Regidx : bits(5)

val add_indices : (regidx, bits(5), regidx) -> bits(5)
function add_indices (Regidx(a), offset, Regidx(b)) = a + offset + b

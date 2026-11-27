default Order dec

$include <prelude.sail>

/*
  A parameter that takes a newtype apart, as sail-riscv's register accessors do.
  It becomes a fresh parameter arg#0 that the body matches on.
*/

newtype regidx = Regidx : bits(5)
newtype regno  = Regno  : range(0, 31)

val rX : regno -> bits(32)
function rX (Regno(r) : regno) = sail_zero_extend(sail_shiftleft(0b1, r), 32)

val rX_bits : regidx -> bits(32)
function rX_bits Regidx(i) = rX(Regno(unsigned(i)))

val wX_bits : (regidx, bits(32)) -> bits(32)
function wX_bits (Regidx(i) : regidx, data : bits(32)) = sail_zero_extend(i, 32) + data

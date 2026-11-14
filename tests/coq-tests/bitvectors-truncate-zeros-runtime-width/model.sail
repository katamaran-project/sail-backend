default Order dec

$include <prelude.sail>


/*
  Zeros of run-time length appended to a bitvector, then truncated to a fixed width
  (as in CHERIoT's getCapBoundsBits): the intermediate width, 9 + e, is only known at run time.
*/
val bounds : (bits(4), bits(5), bits(3)) -> bits(8)
function bounds(hi, mid, e) = {
  let n = unsigned(e);
  truncate(hi @ mid @ sail_zeros(n), 8)
}


/* Same, but with a target width larger than the prefix (12 > 4 + 5) */
val bounds_wide : (bits(4), bits(5), bits(3)) -> bits(12)
function bounds_wide(hi, mid, e) = {
  let n = unsigned(e) + 3;
  truncate(hi @ mid @ sail_zeros(n), 12)
}


/* Shift by a run-time amount */
val shift : (bits(8), bits(3)) -> bits(8)
function shift(x, n) = sail_shiftleft(x, unsigned(n))


/* Truncation to a constant width */
val low : bits(16) -> bits(8)
function low(x) = truncate(x, 8)

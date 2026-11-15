default Order dec

$include <prelude.sail>


/*
  A mask of the n low bits, zero-extended to a fixed width, with n known only at run time
  (as in CHERIoT's setCapBounds): the intermediate bitvector ones(n) has a run-time width.
*/
val mask : bits(3) -> bits(8)
function mask(e) = {
  let n = unsigned(e);
  sail_zero_extend(sail_ones(n), 8)
}


/* sail-riscv's ones and zero_extend, which take their width as an implicit argument */
val ones : forall 'n, 'n >= 0. implicit('n) -> bits('n)
function ones(n) = not_vec(sail_zeros(n))

val zero_extend : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
function zero_extend(m, v) = sail_zero_extend(v, m)


/* With a range-typed variable updated in a branch, as setCapBounds' e_sat */
val low_bits : (bits(9), range(0, 8), bool) -> bool
function low_bits(x, e, more) = {
  var e_sat : range(0, 8) = e;
  if more then e_sat = min_int(e_sat + 1, 8);
  let m : bits(9) = zero_extend(ones(e_sat));
  unsigned(x & m) != 0
}

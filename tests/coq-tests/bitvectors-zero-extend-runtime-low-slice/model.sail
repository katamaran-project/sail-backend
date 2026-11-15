default Order dec
$include <prelude.sail>

val zero_extend : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
function zero_extend(m, v) = sail_zero_extend(v, m)

val get_vlen_pow : unit -> {|3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16|}
function get_vlen_pow() = 7

register vstart : bits(16)

// sail-riscv's writeCSR for vstart: a low slice of run-time length, zero-extended
val write_vstart : bits(32) -> unit
function write_vstart(value) = {
  let vstart_length = get_vlen_pow();
  vstart = zero_extend(16, value[vstart_length - 1 .. 0])
}

// the same, widened instead of truncated
val widen_low : bits(8) -> bits(16)
function widen_low(v) = {
  let n = get_vlen_pow();
  if n <= 8 then zero_extend(16, v[n - 1 .. 0]) else zero_extend(v)
}

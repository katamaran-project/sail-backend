default Order dec
$include <prelude.sail>

// reverses the bytes, as sail-riscv's REV8: a step, slices at run-time indices
val rev8 : bits(32) -> bits(32)
function rev8(v) = {
  result : bits(32) = sail_zeros(32);
  foreach (i from 0 to 24 by 8)
    result[(i + 7) .. i] = v[(32 - i - 1) .. (32 - i - 8)];
  result
}

// interleaves the bits of the halves, as sail-riscv's ZIP: updates of single bits
val zip : bits(32) -> bits(32)
function zip(v) = {
  result : bits(32) = sail_zeros(32);
  foreach (i from 0 to 15) {
    result[i * 2] = v[i];
    result[i * 2 + 1] = v[i + 16];
  };
  result
}

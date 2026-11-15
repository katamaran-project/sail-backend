default Order dec
$include <prelude.sail>

type regidx = bits(5)

// sail-riscv's bool_not_bits and the DIV encoding: decoding matches on a bool
mapping bool_not_bits : bool <-> bits(1) = {
  true   <-> 0b0,
  false  <-> 0b1
}

union ast = { DIV : (regidx, regidx, bool) }

mapping encdec : ast <-> bits(16) = {
  DIV(rs2, rs1, s) <-> 0b00000 @ rs2 @ rs1 @ bool_not_bits(s)
}

val decode : bits(16) -> ast
function decode(b) = encdec(b)

// literal cases, and a binder after a literal case
val pick : bool -> bits(1)
function pick(b) = match b { true => 0b1, false => 0b0 }

val keep : bool -> bool
function keep(b) = match b { false => false, c => c }

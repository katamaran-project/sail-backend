default Order dec

$include <prelude.sail>
$include <smt.sail>

/*
   Externs of Sail's library (and of sail-riscv's prelude) that correspond to muSail
   operations become those operations; the others become foreign functions whose
   semantics the translator knows (integer division, pow2, ...).
   The translator recognises an extern by its binding (the name after _, or else after c),
   not by its Sail name, so the declarations below, copied from sail-riscv, are recognised.
*/

val shiftl = pure {_: "shiftl"} : forall 'm 'n, 'n >= 0. (bits('m), int('n)) -> bits('m)
val shiftr = pure {_: "shiftr"} : forall 'm 'n, 'n >= 0. (bits('m), int('n)) -> bits('m)
val shift_bits_left  = pure {_: "shift_bits_left"}  : forall 'n 'm. (bits('n), bits('m)) -> bits('n)
val shift_bits_right = pure {_: "shift_bits_right"} : forall 'n 'm. (bits('n), bits('m)) -> bits('n)
val sub_vec     = pure {c: "sub_bits", _: "sub_vec"}         : forall 'n. (bits('n), bits('n)) -> bits('n)
val sub_vec_int = pure {c: "sub_bits_int", _: "sub_vec_int"} : forall 'n. (bits('n), int) -> bits('n)
val eq_anything = pure {ocaml: "(fun x y -> x = y)", coq: "generic_eq", _: "eq_anything"} : forall ('a : Type). ('a, 'a) -> bool
val quot_round_zero = pure {ocaml: "quot_round_zero", c: "tdiv_int", coq: "Z.quot"} : (int, int) -> int
val rem_round_zero  = pure {ocaml: "rem_round_zero", c: "tmod_int", coq: "Z.rem"} : (int, int) -> int

enum Mode = { User, Machine }

/* Shifts */

val shift_left_constant : bits(32) -> bits(32)
function shift_left_constant(x) = shiftl(x, 3)

val shift_left_by_four : bits(32) -> bits(32)
function shift_left_by_four(x) = shiftl(x, 4)

val shift_right_by_two : bits(32) -> bits(32)
function shift_right_by_two(x) = shiftr(x, 2)

val shift_right_runtime : (bits(32), nat) -> bits(32)
function shift_right_runtime(x, n) = shiftr(x, n)

val shift_left_by_bits : (bits(32), bits(5)) -> bits(32)
function shift_left_by_bits(x, y) = shift_bits_left(x, y)

val shift_right_by_bits : (bits(32), bits(5)) -> bits(32)
function shift_right_by_bits(x, y) = shift_bits_right(x, y)

/* Subtraction */

val subtract : (bits(16), bits(16)) -> bits(16)
function subtract(x, y) = sub_vec(x, y)

val subtract_int : (bits(16), int) -> bits(16)
function subtract_int(x, n) = sub_vec_int(x, n)

val subtract_constant : bits(16) -> bits(16)
function subtract_constant(x) = sub_vec_int(x, 1)

/* Equality at any type */

val same_bits : (bits(8), bits(8)) -> bool
function same_bits(x, y) = eq_anything(x, y)

val same_mode : (Mode, Mode) -> bool
function same_mode(x, y) = eq_anything(x, y)

/* Bit access, slices */

val bit_3 : bits(8) -> bit
function bit_3(x) = bitvector_access(x, 3)

val bit_at : (bits(8), range(0, 7)) -> bit
function bit_at(x, i) = bitvector_access(x, i)

val low_byte_of : int -> bits(8)
function low_byte_of(n) = get_slice_int(8, n, 0)

val second_byte_of : int -> bits(8)
function second_byte_of(n) = get_slice_int(8, n, 8)

val top_byte : bits(32) -> bits(8)
function top_byte(x) = truncateLSB(x, 8)

/* Integer arithmetic: foreign functions with known semantics */

val euclidean_modulo : (int, int) -> int
function euclidean_modulo(a, b) = emod_int(a, b)

val euclidean_division : (int, int) -> int
function euclidean_division(a, b) = ediv_int(a, b)

val truncating_division : (int, int) -> int
function truncating_division(a, b) = quot_round_zero(a, b)

val truncating_remainder : (int, int) -> int
function truncating_remainder(a, b) = rem_round_zero(a, b)

val power_of_two : nat -> int
function power_of_two(n) = pow2(n)

val minimum : (int, int) -> int
function minimum(a, b) = min_int(a, b)

val maximum : (int, int) -> int
function maximum(a, b) = max_int(a, b)

val leading_zeros : bits(16) -> int
function leading_zeros(x) = count_leading_zeros(x)

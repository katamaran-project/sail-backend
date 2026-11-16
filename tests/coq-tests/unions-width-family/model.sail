default Order dec

$include <prelude.sail>

/*
   A union with a type parameter used at a bitvector of a size variable (Res(bits(8 * 'n)))
   is one Rocq family indexed by the width, like Katamaran's RiscvPmp memory_op_result:

     Inductive Res_bits (kid__n : nat) : Set := Val_bits : bv kid__n -> ... | Exc_bits : bv 4 -> ...
     | Ures_bits (n : nat)                                   (in Unions)

   Its uses at closed widths (Res(bits(32))) are the family at that width.
   Its uses at other types (Res(bool)) are monomorphic copies, as before.
*/

union Res('a : Type) = {
  Val : 'a,
  Exc : bits(4),
}

val read_bytes = pure "read_bytes" : forall 'n, 0 < 'n <= 8. int('n) -> bits(8 * 'n)

val read : forall 'n, 0 < 'n <= 8. int('n) -> Res(bits(8 * 'n))
function read(width) = if width == 3 then Exc(0x1) else Val(read_bytes(width))

val read_word : unit -> bits(32)
function read_word() = match read(4) {
  Val(v) => v,
  Exc(_) => 0x00000000,
}

val pass_on : forall 'n, 0 < 'n <= 8. Res(bits(8 * 'n)) -> bool
function pass_on(r) = match r {
  Val(_) => true,
  Exc(_) => false,
}

val is_ok : Res(bool) -> bool
function is_ok(r) = match r {
  Val(b) => b,
  Exc(_) => false,
}

register last : Res(bits(16)) = Exc(0x0)

/*
   A function polymorphic in a type over the union (Res('t)) cannot be one definition:
   Katamaran's unions cannot be indexed by a Ty.
   - Used at one or two types (a width family counts as one): one copy per type; one copy keeps the name.
   - Used at more types: its calls are inlined.
*/
val add_flag : forall ('t : Type). (Res('t), bool) -> Res(('t, bool))
function add_flag(r, b) = match r {
  Val(v) => Val((v, b)),
  Exc(e) => Exc(e),
}

val drop_flag : forall ('t : Type). Res(('t, bool)) -> Res('t)
function drop_flag(r) = match r {
  Val((v, _)) => Val(v),
  Exc(e) => Exc(e),
}

val read_flagged : forall 'n, 0 < 'n <= 8. int('n) -> Res(bits(8 * 'n))
function read_flagged(width) = drop_flag(add_flag(read(width), true))

// used at two types: copies is_val_bits and is_val_bool
val is_val : forall ('t : Type). Res('t) -> bool
function is_val(r) = match r {
  Val(_) => true,
  Exc(_) => false,
}

val read_is_val : forall 'n, 0 < 'n <= 8. int('n) -> bool
function read_is_val(width) = is_val(read(width))

val bool_is_val : Res(bool) -> bool
function bool_is_val(r) = is_val(r)

// used at three types: inlined
val exc_code : forall ('t : Type). Res('t) -> bits(4)
function exc_code(r) = match r {
  Val(_) => 0x0,
  Exc(e) => e,
}

val code_of_read : forall 'n, 0 < 'n <= 8. int('n) -> bits(4)
function code_of_read(width) = exc_code(read(width))

val code_of_bool : Res(bool) -> bits(4)
function code_of_bool(r) = exc_code(r)

val code_of_int : Res(int) -> bits(4)
function code_of_int(r) = exc_code(r)

// 'n appears in no parameter type, only in 'p8_times_n == 8 * 'n: a caller passes it as p8_times_n / 8
val widen_result : forall 'n, 0 < 'n <= 4. (bool, Res(bits(8 * 'n))) -> Res(bits(32))
function widen_result(is_signed, r) = match r {
  Val(v) => Val(if is_signed then sail_sign_extend(v, 32) else sail_zero_extend(v, 32)),
  Exc(e) => Exc(e),
}

val load : forall 'n, 0 < 'n <= 4. int('n) -> Res(bits(32))
function load(width) = widen_result(true, read(width))

val load_result : forall 'n, 0 < 'n <= 4. (bool, Res(bits(8 * 'n))) -> Res(bits(32))
function load_result(is_signed, r) = widen_result(is_signed, r)

default Order dec

$include <prelude.sail>
$include <string.sail>

/* As in sail-riscv's prelude.sail */
overload operator ^ = {concat_str}

/*
   An assert whose message calls a mapping, shaped like report_invalid_width in
   sail-riscv's riscv_types.sail, which uses size_mnemonic(w).

   Sail considers every mapping possibly failing, so it does not consider this
   message pure. The translation drops it anyway, with a warning.
*/

enum word_width = {BYTE, HALF, WORD, DOUBLE}

mapping size_mnemonic : word_width <-> string = {
  BYTE   <-> "b",
  HALF   <-> "h",
  WORD   <-> "w",
  DOUBLE <-> "d"
}

val check : (bool, word_width) -> unit
function check(condition, w) = {
  assert(condition, "invalid width " ^ size_mnemonic(w))
}

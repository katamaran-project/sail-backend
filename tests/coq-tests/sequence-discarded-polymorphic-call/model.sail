default Order dec
$include <prelude.sail>

enum Retired = { RETIRE_SUCCESS, RETIRE_FAIL }

// sail-riscv's internal_error: its result type is a type variable
val internal_error : forall ('a : Type). (string, int, string) -> 'a
function internal_error(file, line, s) = {
  assert(false, s);
  exit()
}

val describe : bits(16) -> string
function describe(s) = "an encoding"

// cheriot-sail's execute(NOT_C_CAPMODE(s)): the call's result is discarded in a block,
// and its message is computed first (a let around the call)
val not_c_capmode : bits(16) -> Retired
function not_c_capmode(s) = {
  internal_error("cheri_insts_end.sail", 77, describe(s));
  RETIRE_FAIL
}

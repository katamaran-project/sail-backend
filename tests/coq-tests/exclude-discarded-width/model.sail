default Order dec

$include <prelude.sail>

/*
   Values passed to excluded definitions are still computed, then discarded:
   - print_line (no-op) and show (unconstrained): widen(x) in log_byte;
   - the same with a type ascription: widen(x) : bits(32) in log_ascribed;
   - the excluded register scratch: widen(0x00) in reset_scratch.
   widen's result width is an implicit argument that only the type of the
   let around the call, bits(32), fixes; the let must stay although unused.
*/

val widen : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
function widen(m, x) = sail_zero_extend(x, m)

val show : bits(32) -> string

val print_line : string -> unit

val log_byte : bits(8) -> unit
function log_byte(x) = print_line(show(widen(x)))

val log_ascribed : bits(8) -> unit
function log_ascribed(x) = print_line(show(widen(x) : bits(32)))

register scratch : bits(32)

val reset_scratch : unit -> unit
function reset_scratch() = scratch = widen(0x00)

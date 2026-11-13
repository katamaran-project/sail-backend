default Order dec

$include <prelude.sail>

/*
   The fixed-width sibling of ../assert-flow-typing-runtime-width.

   As there, the slice v[n - 1 .. 0] type-checks only because of the assert before it:
   n has type int, so only the assert tells Sail that 0 < n <= 64.
   The plugin's remove_blocks rewrite used to turn the block into nested lets,
   `let _ : unit = assert(...) in ...`, after which the assert no longer constrained n
   and the recheck_defs that followed crashed Sail with
   "Internal error: Unreachable code (at src/lib/type_error.ml line 577)".

   Unlike there, n is a constant, so the slice has a width known at compile time
   and the whole function can be translated to muSail.
   This test guards the fix of the crash; ../assert-flow-typing-runtime-width additionally
   needs bitvectors whose width is only known at run time.
*/

val low_bits : bits(64) -> unit
function low_bits(v) = {
  let n : int = 32;
  assert(0 < n & n <= 64);
  let w = v[n - 1 .. 0];
  ()
}

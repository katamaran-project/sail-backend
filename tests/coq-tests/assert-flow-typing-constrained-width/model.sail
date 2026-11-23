default Order dec

$include <prelude.sail>

/*
   A sibling of ../expected-failing/assert-flow-typing-runtime-width and ../assert-flow-typing-fixed-width.

   n is read from a register, so it is a run-time value, but the assert pins it to 32:
   after the assert, Sail knows n == 32, so the slice v[n - 1 .. 0] has type bits(32),
   a width known at compile time.

   The slice's index n - 1 is not a literal, but Sail's solver knows it is 31: the translation
   replaces it by that value (rewrite_solved_indices), and the width of w by 32.
*/

register vlenb : bits(8)

val low_bits : bits(64) -> unit
function low_bits(v) = {
  let n = unsigned(vlenb);
  assert(n == 32);
  let w = v[n - 1 .. 0];
  ()
}

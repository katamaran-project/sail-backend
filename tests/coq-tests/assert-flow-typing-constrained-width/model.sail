default Order dec

$include <prelude.sail>

/*
   A sibling of ../assert-flow-typing-runtime-width and ../assert-flow-typing-fixed-width.

   n is read from a register, so it is a run-time value, but the assert pins it to 32:
   after the assert, Sail knows n == 32, so the slice v[n - 1 .. 0] has type bits(32),
   a width known at compile time.

   Status: expected to fail. Sail and the translation to nanoSail handle it, but the
   generation of muSail only accepts slice indices that are constants in the code,
   and does not use the constraint n == 32, so the slice is generated as
   `uop.vector_subrange NYI NYI` (see FunctionCalls.ml, "indices should be known at compile time").
   It should pass once the code generator takes the constraints of the type checker into
   account; that does not need bitvectors whose width is only known at run time.
*/

register vlenb : bits(8)

val low_bits : bits(64) -> unit
function low_bits(v) = {
  let n = unsigned(vlenb);
  assert(n == 32);
  let w = v[n - 1 .. 0];
  ()
}

default Order dec

$include <prelude.sail>

/*
   The slice v[n - 1 .. 0] type-checks only because of the assert before it:
   Sail's flow typing adds 0 < n <= 64 to what it knows for the rest of the block.
   The plugin's remove_blocks rewrite turns the block into nested lets, so the
   assert becomes `let _ : unit = assert(...) in ...` and no longer constrains n.
   The recheck_defs that follows then crashes Sail with
   "Internal error: Unreachable code (at src/lib/type_error.ml line 577)".
   Reduced from wV in sail-riscv's riscv_vext_regs.sail, which breaks the CHERIoT model.

   Status: the crash is fixed (remove_blocks is no longer used; ../assert-flow-typing-fixed-width
   guards that). This test is expected to fail until muSail translation supports bitvectors
   whose width is only known at run time: the slice has width n, a run-time value,
   and is translated to `uop.vector_subrange NYI NYI`.
*/

register vlenb : bits(8)

val low_bits : bits(64) -> unit
function low_bits(v) = {
  let n = unsigned(vlenb);
  assert(0 < n & n <= 64);
  let w = v[n - 1 .. 0];
  ()
}

default Order dec

$include <prelude.sail>

/*
   (ignore-functions '("low_bits")) ignores low_bits by name, on top of the
   ignore predicate that ../shared-configuration.lisp sets, even though it comes
   before it. low_bits slices a bitvector with a width known only at run time,
   which generates NYI holes (see ../assert-flow-typing-runtime-width), so this
   test passes only if low_bits is ignored. The CHERIoT loop (tests/cheriot/run.py)
   ignores the definitions outside its scope this way.
*/

register vlenb : bits(8)

val low_bits : bits(64) -> unit
function low_bits(v) = {
  let n = unsigned(vlenb);
  assert(0 < n & n <= 64);
  let w = v[n - 1 .. 0];
  ()
}

val kept : bits(8) -> bits(8)
function kept(v) = v

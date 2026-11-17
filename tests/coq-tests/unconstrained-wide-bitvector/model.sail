default Order dec

$include <prelude.sail>

/*
   Unconstrained values at a 65536-bit width, as CHERIoT's vector registers
   (`type vregtype = bits(65536)`). Rocq finds the inhabitant that
   `unconstrained σ` needs by type class search; at this width it must not
   reduce the width (a unary `nat`) before building the value.
   The union holds the wide bitvector through a record: a union field of
   this width makes `base.v` slow on its own.
*/

type vregtype = bits(65536)

struct vpair = { lo : vregtype, hi : vregtype }

union vresult = { VOk : vpair, VErr : bits(8) }

val vread : unit -> vregtype

val vpair_read : unit -> vpair

val vresult_read : unit -> vresult

val vtuple_read : unit -> (vregtype, bool)

val use_all : unit -> bool
function use_all() = {
  let v = vread();
  let p = vpair_read();
  let r = vresult_read();
  let (t, b) = vtuple_read();
  b
}

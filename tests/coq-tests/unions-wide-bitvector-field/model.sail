default Order dec

$include <prelude.sail>

/*
   Union fields at a 65536-bit width, as CHERIoT's vector registers
   (`type vregtype = bits(65536)`): directly, in a pair and a triple, and
   next to a narrow field. The union's fold/unfold proof must not reduce the width
   (a unary `nat`).
*/

type vregtype = bits(65536)

union vresult = {
  VOk     : vregtype,
  VPair   : (vregtype, bits(8)),
  VTriple : (vregtype, bits(8), bool),
  VErr    : bits(8),
}

val is_ok : vresult -> bool
function is_ok(r) = match r {
  VOk(_)     => true,
  VPair(_)   => false,
  VTriple(_) => false,
  VErr(_)    => false,
}

default Order dec

$include <prelude.sail>

/*
   Bitvector types whose width is an arithmetic expression. In Rocq the width
   is an argument of bv, so it must be parenthesised: bv 8 * 8 parses as
   (bv 8) * 8. The CHERIoT model defines its capability types this way, e.g.
   type CapBits = bits(8 * cap_size).
*/

type cap_size : Int = 8
type cap_addr_width : Int = 32

type CapBits = bits(8 * cap_size)
type CapLenBits = bits(cap_addr_width + 1)
type low_bits = bits(cap_addr_width - 3)
type nested = bits(2 * (cap_size + 1))

val shorten : CapLenBits -> low_bits
function shorten(v) = v[28 .. 0]

default Order dec
$include <prelude.sail>

// length(v) in a size-polymorphic function, as sail-riscv's rotate_bits_right:
// a foreign call whose semantics is the width, not an unconstrained value
val rotate_right : forall 'n, 'n > 0. (bits('n), int) -> bits('n)
function rotate_right(v, n) = sail_shiftright(v, n) | sail_shiftleft(v, length(v) - n)

val rotate_right_32 : (bits(32), int) -> bits(32)
function rotate_right_32(v, n) = rotate_right(v, n)

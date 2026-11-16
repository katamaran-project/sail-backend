default Order dec
$include <prelude.sail>

val zeros_implicit : forall 'n, 'n >= 0 . implicit('n) -> bits('n)
function zeros_implicit (n) = sail_zeros(n)
overload zeros = {zeros_implicit}

// cheriot-sail's align_down: the updated range's width is the size variable 'n
val align_down : forall 'n 'm, 'n >= 1 & 'm > 'n . (int('n), bits('m)) -> bits('m)
function align_down(n, bv) = [bv with ('n - 1) .. 0 = zeros()]

val granule : bits(32) -> bits(32)
function granule(base) = align_down(3, base)

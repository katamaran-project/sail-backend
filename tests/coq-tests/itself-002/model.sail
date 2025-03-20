default Order dec

$include <prelude.sail>


val index : forall 'n 'i, 0 <= 'i & 'i < 'n. (bits('n), int('i)) -> bit
function index(bv, idx) = bv[idx]

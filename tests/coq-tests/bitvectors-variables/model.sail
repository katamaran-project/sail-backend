default Order dec
$include <prelude.sail>
$include <option.sail>

type mybits('n) = bitvector('n)

struct S('m) = { value : mybits(8 * 'm) }

val f : forall 'n, 'n > 0. bits('n) -> bits('n)
function f(x) = x

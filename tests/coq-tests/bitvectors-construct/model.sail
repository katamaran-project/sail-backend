default Order dec

$include <prelude.sail>


val mkbv : forall 'n, 'n > 0. int('n) -> bits('n)
function mkbv(n) =
  sail_zero_extend(0b0, n)


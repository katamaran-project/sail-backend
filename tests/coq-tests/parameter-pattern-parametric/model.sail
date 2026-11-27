default Order dec

$include <prelude.sail>

/*
  A parameter that takes apart a newtype with a type parameter.
*/

newtype sized('n) = Sized : bits('n)

val payload : forall 'n, 'n >= 0. (int('n), sized('n)) -> bits('n)
function payload (n, Sized(v)) = v

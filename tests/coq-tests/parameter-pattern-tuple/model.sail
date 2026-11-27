default Order dec

$include <prelude.sail>

/*
  A parameter that is a tuple pattern inside the parameter list.
*/

val sum3 : ((bits(8), bits(8)), bits(8)) -> bits(8)
function sum3 ((a, b), c) = a + b + c

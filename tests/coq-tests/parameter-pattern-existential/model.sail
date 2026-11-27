default Order dec

$include <prelude.sail>

/*
  A parameter pattern that binds a type variable of an existential payload ('m).
  The body is not type-checked again after the pattern moves into it, so this needs no special case.
*/

union wrapped = { Wrap : {'n, 'n in {8, 16}. int('n)} }

val unwrap : wrapped -> int
function unwrap Wrap(x as int('m)) = x

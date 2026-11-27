default Order dec

$include <prelude.sail>

/*
  A parameter that takes a struct apart.
*/

struct pair = { first : bits(8), second : bits(8) }

val first_of : pair -> bits(8)
function first_of (struct { first = f, _ }) = f

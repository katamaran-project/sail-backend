default Order dec
$include <prelude.sail>
$include <string.sail>
$include <smt.sail> //division modulo and absolute value




// bitshift operator
val shiftl = pure "shiftl" : forall 'm. (bits('m), int) -> bits('m)

infixl 7 <<
overload operator << = { shiftl }

val shiftr = pure "shiftr" : forall 'm. (bits('m), int) -> bits('m)

infixl 7 >>
overload operator >> = { shiftr }

// adding the division and modulo operators
overload operator / = {ediv_int}
overload operator % = {emod_int}

// making a funtion that gets the negative value for our
val neg_vec4: (bits(4)) -> bits(4)
function neg_vec4(v) = sub_bits(0x0, v)

overload neg = {neg_vec4}

overload operator + = {concat_str}

val __GetVerbosity = monadic {c: "sail_get_verbosity"}: unit -> bits(64)

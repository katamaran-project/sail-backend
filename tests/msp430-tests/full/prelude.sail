default Order dec
$include <prelude.sail>
$include <string.sail>
$include <smt.sail> //division modulo and absolute value



// extension for bitvectors of all lengths
val sign_extend : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
val zero_extend : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
function sign_extend(m, v) = sail_sign_extend(v, m)
function zero_extend(m, v) = sail_zero_extend(v, m)


// bitshift operator
val shiftl = pure "shiftl" : forall 'm. (bits('m), int) -> bits('m)

infixl 7 <<
overload operator << = { shiftl }

// adding the division and modulo operators
overload operator / = {ediv_int}
overload operator % = {emod_int}

// making a funtion that gets the negative value for our
val neg_vec: forall 'n, 0 < 'n. (bits('n), int('n)) -> bits('n)
function clause neg_vec(a, n) = {
  let r = 0b0 in
  sub_bits(sail_zero_extend(r, n), a)
}

overload neg = {neg_vec}

overload operator + = {concat_str}

val __GetVerbosity = monadic {c: "sail_get_verbosity"}: unit -> bits(64)
default Order dec
$include <prelude.sail>
$include <string.sail>
$include <smt.sail> //division modulo and absulute value
$include <elf.sail>


//extention for bitvectors of all lengths
val sign_extend : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
val zero_extend : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
function sign_extend(m, v) = sail_sign_extend(v, m)
function zero_extend(m, v) = sail_zero_extend(v, m)


//bitshift opperator
infixl 7 <<
val operator << = pure "shiftl" : forall 'm. (bits('m), int) -> bits('m)
val shiftl = pure "shiftl" : forall 'm. (bits('m), int) -> bits('m)

// adding the division and modulo operators
overload operator / = {ediv_int}
overload operator % = {emod_int}

//making a funtion that gets the negative value for our
val neg_vec: forall 'n, 0 < 'n.  (bits('n)) -> bits('n)
function clause neg_vec(a) = {
  let r = 0b0 in
  sub_bits(zero_extend(r), a)
}

overload neg = {neg_vec}

overload operator + = {concat_str}

val __GetVerbosity = {c: "sail_get_verbosity"}: unit -> bits(64)
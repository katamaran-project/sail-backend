val toByte: (WordByte) -> WordByte
function clause toByte(Word(w)) = {
  Byte(w[7 .. 0])
}
function clause toByte(Byte(b)) = {
  Byte(b)
}

val signedWb: (WordByte) -> int
function clause signedWb(Word(w)) = {
  signed(w)
}
function clause signedWb(Byte(b)) = {
  signed(b)
}

val unsignedWb: (WordByte) -> int
function clause unsignedWb(Word(w)) = {
  unsigned(w)
}
function clause unsignedWb(Byte(b)) = {
  unsigned(b)
}

val addBw: (WordByte, WordByte) -> WordByte
function clause addBw(Byte(b1), Byte(b2)) = {
  Byte(b1 + b2)
}
function clause addBw(Word(w1), Word(w2)) = {
  Word(w1 + w2)
}
function clause addBw(Word(w1), Byte(b2)) = {
  Word(w1 + zero_extend(b2))
}
function clause addBw(Byte(b1), Word(w2)) = {
  Word(w2 + zero_extend(b1))
}

overload operator + = {addBw}

val W_zero_extend : forall 'n, 8 < 'n & 'n <= 16. (bits('n)) -> WordByte
function W_zero_extend(b) = {
  let bts:bits(16) = zero_extend(b) in
  Word(bts)
}
val B_zero_extend : forall 'n, 'n <= 8. (bits('n)) -> WordByte
function B_zero_extend(b) = {
  let bts:bits(8) = zero_extend(b) in
  Byte(bts)
}

overload BW_zero_extend = {W_zero_extend, B_zero_extend}


val W_sign_extend : forall 'n, 8 < 'n & 'n <= 16. (bits('n)) -> WordByte
function W_sign_extend(b) = {
  let bts:bits(16) = sign_extend(b) in
  Word(bts)
}
val B_sign_extend : forall 'n, 'n <= 8. (bits('n)) -> WordByte
function B_sign_extend(b) = {
  let bts:bits(8) = sign_extend(b) in
  Byte(bts)
}

overload BW_sign_extend = {W_sign_extend, B_sign_extend}



val not_wordByte: (WordByte) -> WordByte
function clause not_wordByte(Word(w)) = {
  Word(not_vec(w))
}
function clause not_wordByte(Byte(b)) = {
  Byte(not_vec(b))
}
overload ~ = {not_bool, not_vec, not_wordByte}


val and_wordByte: (WordByte,WordByte) -> WordByte
function clause and_wordByte(Word(w1),Word(w2)) = {
  Word(and_vec(w1,w2))
}
function clause and_wordByte(Byte(b1),Byte(b2)) = {
  Byte(and_vec(b1,b2))
}

function clause and_wordByte(Word(_),Byte(_)) = {
  throw(notImplemented("not implemented"))
}

function clause and_wordByte(Byte(_),Word(_)) = {
  throw(notImplemented("not implemented"))
}

val or_wordByte: (WordByte,WordByte) -> WordByte
function clause or_wordByte(Word(w1),Word(w2)) = {
  Word(or_vec(w1,w2))
}
function clause or_wordByte(Byte(b1),Byte(b2)) = {
  Byte(or_vec(b1,b2))
}

function clause or_wordByte(Byte(_),Word(_)) = {
  throw(notImplemented("not implemented"))
}

function clause or_wordByte(Word(_),Byte(_)) = {
  throw(notImplemented("not implemented"))
}

val xor_wordByte: (WordByte,WordByte) -> WordByte
function clause xor_wordByte(Word(w1),Word(w2)) = {
  Word(xor_vec(w1,w2))
}

function clause xor_wordByte(Byte(b1),Byte(b2)) = {
  Byte(xor_vec(b1,b2))
}

function clause xor_wordByte(Word(_),Byte(_)) = {
  throw(notImplemented("not implemented"))
}

function clause xor_wordByte(Byte(_),Word(_)) = {
  throw(notImplemented("not implemented"))
}

val eq_wordByte: (WordByte,WordByte) -> bool
function clause eq_wordByte(Byte(b1),Byte(b2)) = {
  b1 == b2
}
function clause eq_wordByte(Word(w1),Word(w2)) = {
  w1 == w2
}

function clause eq_wordByte(_,_) = {
  false
}

val xor_bool: (bool, bool) -> bool
function xor_bool(b1,b2) = {
  or_bool(and_bool(~(b1), b2), and_bool(b1, ~(b2)))
}

val isNegative : (WordByte) -> bool
function isNegative(w) = {
  signedWb(w) < 0
}

val isZero : (WordByte) -> bool
function isZero(w) = {
  signedWb(w) == 0
}

overload operator & = {and_bool, and_vec, and_wordByte}

overload operator | = {or_bool, or_vec, or_wordByte}

overload operator ^ = {xor_bool, xor_vec, xor_wordByte}

overload operator == = {eq_wordByte}

val printWordByte : (string, WordByte) -> unit
function printWordByte(s, wb) = {
    match wb{
        Word(x) => print_bits(s, x),
        Byte(x) => print_bits(s, x)
    }
}

val WordByteString : (WordByte) -> string
function WordByteString(wb) = {
    match wb{
        Word(x) => bits_str(x),
        Byte(x) => bits_str(x)
    }
}
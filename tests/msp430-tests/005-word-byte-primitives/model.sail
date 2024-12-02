default Order dec
$include <prelude.sail>
$include <string.sail>
$include <smt.sail>
$include "generated.sail"

scattered union exception
union clause exception = notImplemented : string
union clause exception = notAllowed : string
union clause exception = undefindedBehavior : string
union clause exception = undefindedInstruction : bits(16)

type len_word: Int = 16
type len_byte: Int = 8
type wordBits      = bits(len_word)
type byteBits      = bits(len_byte)

enum BW = {
    WORD_INSTRUCTION,
    BYTE_INSTRUCTION
}
mapping bitmaping: BW <-> bits(1) = {
    WORD_INSTRUCTION <-> 0b0,
    BYTE_INSTRUCTION <-> 0b1
}

scattered union WordByte
union clause WordByte = Byte : byteBits
union clause WordByte = Word : wordBits

type registerAddressLen:  Int  = 4
type registerAddressBits       = bits(registerAddressLen)

type address_size : Int = 16 //CHECK IF FAIL: note in example says this can not be 16 but it seems to work
type Address = bits(address_size)


type addressingModeSourceLen : Int = 2
type addressingModeSourceBits = bits(addressingModeSourceLen)

type addressingModeDestinationLen : Int = 1
type addressingModeDestinationBits = bits(addressingModeDestinationLen)

enum Register = {
  PC,
  SP,
  SRCG1,
  CG2,
  R4,
  R5,
  R6,
  R7,
  R8,
  R9,
  R10,
  R11,
  R12,
  R13,
  R14,
  R15,
}

enum AM = {
    REGISTER_MODE,
    INDEXED_MODE,
    INDIRECT_REGISTER_MODE,
    INDIRECT_AUTOINCREMENT_MODE,
}

enum doubleop = {
  MOV,
  ADD,
  ADDC,
  SUB,
  SUBC,
  CMP,
  DADD,
  BIT,
  BIC,
  BIS,
  XOR,
  AND}    /* Double-operand Instructions: Format I */

enum singleop = {
  RRC,
  RRA,
  PUSH,
  SWPB,
  CALL,
  RETI,
  SXT}    /* Single-operand Instructions: Format II */

enum jump = {
  JEQ,
  JNE,
  JC ,
  JNC,
  JN ,
  JGE,
  JL ,
  JMP}    /* Jump-operand Instructions: Format III */

let zero_reg : wordBits = 0b0000000000000000

register    PC_reg  : wordBits = zero_reg
register    SP_reg  : wordBits = zero_reg
register SRCG1_reg  : wordBits = zero_reg
register   CG2_reg  : wordBits = zero_reg
register    R4_reg  : wordBits = zero_reg
register    R5_reg  : wordBits = zero_reg
register    R6_reg  : wordBits = zero_reg
register    R7_reg  : wordBits = zero_reg
register    R8_reg  : wordBits = zero_reg
register    R9_reg  : wordBits = zero_reg
register   R10_reg  : wordBits = zero_reg
register   R11_reg  : wordBits = zero_reg
register   R12_reg  : wordBits = zero_reg
register   R13_reg  : wordBits = zero_reg
register   R14_reg  : wordBits = zero_reg
register   R15_reg  : wordBits = zero_reg

mapping RegisterMapping: Register <-> registerAddressBits  = {
        PC    <-> 0b0000,
        SP    <-> 0b0001,
        SRCG1 <-> 0b0010,
        CG2   <-> 0b0011,
        R4    <-> 0b0100,
        R5    <-> 0b0101,
        R6    <-> 0b0110,
        R7    <-> 0b0111,
        R8    <-> 0b1000,
        R9    <-> 0b1001,
        R10   <-> 0b1010,
        R11   <-> 0b1011,
        R12   <-> 0b1100,
        R13   <-> 0b1101,
        R14   <-> 0b1110,
        R15   <-> 0b1111,
}

val toByte: (WordByte) -> WordByte
function clause toByte(Word(w)) = {
  Byte(w[7 .. 0])
}
function clause toByte(Byte(b)) = {
  Byte(b)
}

/* val signedWb: (WordByte) -> int */
/* function clause signedWb(Word(w)) = { */
/*   signed(w) */
/* } */
/* function clause signedWb(Byte(b)) = { */
/*   signed(b) */
/* } */

/* val unsignedWb: (WordByte) -> int */
/* function clause unsignedWb(Word(w)) = { */
/*   unsigned(w) */
/* } */
/* function clause unsignedWb(Byte(b)) = { */
/*   unsigned(b) */
/* } */

/* val addBw: (WordByte, WordByte) -> WordByte */
/* function clause addBw(Byte(b1), Byte(b2)) = { */
/*   Byte(b1 + b2) */
/* } */
/* function clause addBw(Word(w1), Word(w2)) = { */
/*   Word(w1 + w2) */
/* } */
/* function clause addBw(Word(w), Byte(b)) = { */
/*   Word(w + sail_zero_extend(b, 16)) */
/* } */
/* function clause addBw(Byte(b), Word(w)) = { */
/*   Word(w + sail_zero_extend(b, 16)) */
/* } */

/* overload operator + = {addBw} */

/* val W_zero_extend : forall 'n, 8 < 'n & 'n <= 16. (bits('n)) -> WordByte */
/* function W_zero_extend(b) = { */
/*   let bts:bits(16) = sail_zero_extend(b, 16) in */
/*   Word(bts) */
/* } */
/* val B_zero_extend : forall 'n, 'n <= 8. (bits('n)) -> WordByte */
/* function B_zero_extend(b) = { */
/*   let bts:bits(8) = sail_zero_extend(b, 8) in */
/*   Byte(bts) */
/* } */

/* overload BW_zero_extend = {W_zero_extend, B_zero_extend} */


/* val W_sign_extend : forall 'n, 8 < 'n & 'n <= 16. (bits('n)) -> WordByte */
/* function W_sign_extend(b) = { */
/*   let bts:bits(16) = sail_sign_extend(b, 16) in */
/*   Word(bts) */
/* } */
/* val B_sign_extend : forall 'n, 'n <= 8. (bits('n)) -> WordByte */
/* function B_sign_extend(b) = { */
/*   let bts:bits(8) = sail_sign_extend(b, 8) in */
/*   Byte(bts) */
/* } */

/* overload BW_sign_extend = {W_sign_extend, B_sign_extend} */



/* val not_wordByte: (WordByte) -> WordByte */
/* function clause not_wordByte(Word(w)) = { */
/*   Word(not_vec(w)) */
/* } */
/* function clause not_wordByte(Byte(b)) = { */
/*   Byte(not_vec(b)) */
/* } */
/* overload ~ = {not_bool, not_vec, not_wordByte} */


/* val and_wordByte: (WordByte,WordByte) -> WordByte */
/* function clause and_wordByte(Word(w1),Word(w2)) = { */
/*   Word(and_vec(w1,w2)) */
/* } */
/* function clause and_wordByte(Byte(b1),Byte(b2)) = { */
/*   Byte(and_vec(b1,b2)) */
/* } */

/* function clause and_wordByte(_,_) = { */
/*   throw(notImplemented("not implemented")) */
/* } */

/* val or_wordByte: (WordByte,WordByte) -> WordByte */
/* function clause or_wordByte(Word(w1),Word(w2)) = { */
/*   Word(or_vec(w1,w2)) */
/* } */
/* function clause or_wordByte(Byte(b1),Byte(b2)) = { */
/*   Byte(or_vec(b1,b2)) */
/* } */

/* function clause or_wordByte(_,_) = { */
/*   throw(notImplemented("not implemented")) */
/* } */

/* val xor_wordByte: (WordByte,WordByte) -> WordByte */
/* function clause xor_wordByte(Word(w1),Word(w2)) = { */
/*   Word(xor_vec(w1,w2)) */
/* } */

/* function clause xor_wordByte(Byte(b1),Byte(b2)) = { */
/*   Byte(xor_vec(b1,b2)) */
/* } */

/* function clause xor_wordByte(_,_) = { */
/*   throw(notImplemented("not implemented")) */
/* } */

/* val eq_wordByte: (WordByte,WordByte) -> bool */
/* function clause eq_wordByte(Byte(b1),Byte(b2)) = { */
/*   b1 == b2 */
/* } */
/* function clause eq_wordByte(Word(w1),Word(w2)) = { */
/*   w1 == w2 */
/* } */

/* function clause eq_wordByte(_,_) = { */
/*   false */
/* } */

/* val xor_bool: (bool, bool) -> bool */
/* function xor_bool(b1,b2) = { */
/*   or_bool(and_bool(~(b1), b2), and_bool(b1, ~(b2))) */
/* } */

/* val isNegative : (WordByte) -> bool */
/* function isNegative(w) = { */
/*   signedWb(w) < 0 */
/* } */

/* val isZero : (WordByte) -> bool */
/* function isZero(w) = { */
/*   signedWb(w) == 0 */
/* } */

/* overload operator & = {and_bool, and_vec, and_wordByte} */

/* overload operator | = {or_bool, or_vec, or_wordByte} */

/* overload operator ^ = {xor_bool, xor_vec, xor_wordByte} */

/* overload operator == = {eq_wordByte} */

/* val printWordByte : (string, WordByte) -> unit */
/* function printWordByte(s, wb) = { */
/*     match wb{ */
/*         Word(x) => print_bits(s, x), */
/*         Byte(x) => print_bits(s, x) */
/*     } */
/* } */

/* val WordByteString : (WordByte) -> string */
/* function WordByteString(wb) = { */
/*     match wb{ */
/*         Word(x) => bits_str(x), */
/*         Byte(x) => bits_str(x) */
/*     } */
/* } */
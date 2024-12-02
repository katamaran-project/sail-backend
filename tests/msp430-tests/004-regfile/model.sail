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

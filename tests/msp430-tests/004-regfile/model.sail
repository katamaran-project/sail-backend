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

let zero_reg : wordBits = zero_extend(0x0)

register    PC_reg  : wordBits /* program counter    */
register    SP_reg  : wordBits /* stack pointer      */
register SRCG1_reg  : wordBits /* status             */
register   CG2_reg  : wordBits /* constant generator */
register    R4_reg  : wordBits /* general purpose    */
register    R5_reg  : wordBits /* general purpose    */
register    R6_reg  : wordBits /* general purpose    */
register    R7_reg  : wordBits /* general purpose    */
register    R8_reg  : wordBits /* general purpose    */
register    R9_reg  : wordBits /* general purpose    */
register   R10_reg  : wordBits /* general purpose    */
register   R11_reg  : wordBits /* general purpose    */
register   R12_reg  : wordBits /* general purpose    */
register   R13_reg  : wordBits /* general purpose    */
register   R14_reg  : wordBits /* general purpose    */
register   R15_reg  : wordBits /* general purpose    */

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

val init_base_regs: unit -> unit
function init_base_regs() = {
    PC_reg     = undefined; /* needs to be the address located at 0FFFEh */
    SP_reg     = undefined; /* needs to be initialized by the user */
    SRCG1_reg  = undefined;
    CG2_reg    = undefined;
    R4_reg     = undefined;
    R5_reg     = undefined;
    R6_reg     = undefined;
    R7_reg     = undefined;
    R8_reg     = undefined;
    R9_reg     = undefined;
    R10_reg    = undefined;
    R11_reg    = undefined;
    R12_reg    = undefined;
    R13_reg    = undefined;
    R14_reg    = undefined;
    R15_reg    = undefined;
}
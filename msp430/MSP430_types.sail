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

type registerAdressLen:  Int  = 4
type registerAdressBits       = bits(registerAdressLen)

type address_size : Int = 16 //CHECK IF FAIL: note in example says this can not be 16 but it seems to work
type Address = bits(address_size)


type adressingModeSourceLen : Int = 2
type adressingModeSourceBits = bits(adressingModeSourceLen)

type adressingModeDestinationLen : Int = 1
type adressingModeDestinationBits = bits(adressingModeDestinationLen)

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
  AND}    /* Double-opperand Instructions: Format I */

enum singleop = {
  RRC,
  RRA,
  PUSH,
  SWPB,
  CALL,
  RETI,
  SXT}    /* Single-opperand Instructions: Format II */

enum jump = {
  JEQ,
  JNE,
  JC ,
  JNC,
  JN ,
  JGE,
  JL ,
  JMP}    /* Jump-opperand Instructions: Format III */
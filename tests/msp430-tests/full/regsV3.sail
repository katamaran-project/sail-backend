//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=49
//3.3 Addressing Modes
mapping sourcemaping: AM <-> AddressingModeSourceBits = {
  REGISTER_MODE <-> 0b00,
  INDEXED_MODE <-> 0b01,
  INDIRECT_REGISTER_MODE <-> 0b10,
  INDIRECT_AUTOINCREMENT_MODE <-> 0b11,
}

//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=49
//3.3 Addressing Modes
mapping destinationmaping: AM <-> AddressingModeDestinationBits = {
  REGISTER_MODE <-> 0b0,
  INDEXED_MODE <-> 0b1,
}

register    LastInstructionFetch  : WordByte = Byte(0b00000000) /* program counter    */

val readReg: (BW, Register) -> WordByte
function clause readReg(WORD_INSTRUCTION, reg) = {
  let value: wordBits = match reg {
    PC    =>    PC_reg,
    SP    =>    SP_reg,
    SRCG1 => SRCG1_reg,
    CG2   =>   CG2_reg,
    R4    =>    R4_reg,
    R5    =>    R5_reg,
    R6    =>    R6_reg,
    R7    =>    R7_reg,
    R8    =>    R8_reg,
    R9    =>    R9_reg,
    R10   =>   R10_reg,
    R11   =>   R11_reg,
    R12   =>   R12_reg,
    R13   =>   R13_reg,
    R14   =>   R14_reg,
    R15   =>   R15_reg,
  } in
  Word(value)
}

function clause readReg(BYTE_INSTRUCTION, reg) = {
  let Word(value) : WordByte = readReg(WORD_INSTRUCTION, reg) in
  Byte(value[7 .. 0])
}

val writeReg : (BW, Register, WordByte) -> unit
function clause writeReg(WORD_INSTRUCTION, reg, Word(wb)) = {
  match reg {
    PC    =>    PC_reg = wb,
    SP    =>    SP_reg = wb,
    SRCG1 => SRCG1_reg = wb,
    CG2   =>   CG2_reg = wb,
    R4    =>    R4_reg = wb,
    R5    =>    R5_reg = wb,
    R6    =>    R6_reg = wb,
    R7    =>    R7_reg = wb,
    R8    =>    R8_reg = wb,
    R9    =>    R9_reg = wb,
    R10   =>   R10_reg = wb,
    R11   =>   R11_reg = wb,
    R12   =>   R12_reg = wb,
    R13   =>   R13_reg = wb,
    R14   =>   R14_reg = wb,
    R15   =>   R15_reg = wb,
  }
}
function clause writeReg(BYTE_INSTRUCTION, reg, Word(wbTolong)) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=48
  //3.2.5 General-Purpose Registers R4 to R15
  //writing in all modes will override the high byte
  let wb:bits(16) = sail_zero_extend(wbTolong[7..0], 16);
  match reg {
    PC    =>    PC_reg = wb,
    SP    =>    SP_reg = wb,
    SRCG1 => SRCG1_reg = wb,
    CG2   =>   CG2_reg = wb,
    R4    =>    R4_reg = wb,
    R5    =>    R5_reg = wb,
    R6    =>    R6_reg = wb,
    R7    =>    R7_reg = wb,
    R8    =>    R8_reg = wb,
    R9    =>    R9_reg = wb,
    R10   =>   R10_reg = wb,
    R11   =>   R11_reg = wb,
    R12   =>   R12_reg = wb,
    R13   =>   R13_reg = wb,
    R14   =>   R14_reg = wb,
    R15   =>   R15_reg = wb,
  }
}
function clause writeReg(bw, reg, Byte(bb)) = {
  writeReg(bw, reg, Word(zero_extend(bb)))
}

overload Xreg = {readReg, writeReg}

/*
Todo: separate out the constant generators => readability (maybe)
*/

val read: (BW, Register, AM) -> WordByte
val write: (BW, Register, AM, WordByte) -> unit

/////////////////
//Register mode//
/////////////////

function clause read(BYTE_INSTRUCTION, SRCG1, REGISTER_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=47
  //3.2.3 Status Register (SR)
  throw(notAllowed("Status register can not be read by byte operations"))}

function clause read(bw, CG2, REGISTER_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=47
  //3.2.4 Constant Generator Registers CG1 and CG2
  match bw {
    WORD_INSTRUCTION => Word(0x0000),
    BYTE_INSTRUCTION => Byte(0x00)}}

function clause read(bw, reg, REGISTER_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=50
  //3.3.1 Register Mode
  Xreg(bw, reg)}

////////////////
//Indexed mode//
////////////////
function clause read(bw, SRCG1, INDEXED_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=47
  //3.2.4 Constant Generator Registers CG1 and CG2
  LastInstructionFetch = fetch();
  let Word(address) : WordByte = LastInstructionFetch in
    XmemInstruction(bw,address)}

function clause read(bw, CG2, INDEXED_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=47
  //3.2.4 Constant Generator Registers CG1 and CG2
  match bw {
    WORD_INSTRUCTION => Word(0x0001),
    BYTE_INSTRUCTION => Byte(0x01)}}

function clause read(bw, reg, INDEXED_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=51
  //3.3.2 Indexed Mode
  let Word(initialAddress) : WordByte = read(WORD_INSTRUCTION,reg,REGISTER_MODE) in {
    LastInstructionFetch = fetch();
    let Word(addinigAddress) : WordByte = LastInstructionFetch in
    XmemInstruction(bw,initialAddress + addinigAddress)
  }}

//////////////////////////
//Indirect register mode//
//////////////////////////

function clause read(bw, SRCG1, INDIRECT_REGISTER_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=47
  //3.2.4 Constant Generator Registers CG1 and CG2
  match bw {
    WORD_INSTRUCTION => Word(0x0004),
    BYTE_INSTRUCTION => Byte(0x04)}}

function clause read(bw, CG2, INDIRECT_REGISTER_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=47
  //3.2.4 Constant Generator Registers CG1 and CG2
  match bw {
    WORD_INSTRUCTION => Word(0x0002),
    BYTE_INSTRUCTION => Byte(0x02)}}

function clause read(bw, reg, INDIRECT_REGISTER_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=54
  //3.3.5 Indirect Register Mode
  let Word(address) : WordByte = read(WORD_INSTRUCTION,reg,REGISTER_MODE) in
    XmemInstruction(bw,address)}

///////////////////////////////
//Indirect Autoincrement Mode//
///////////////////////////////

function clause read(bw, SRCG1, INDIRECT_AUTOINCREMENT_MODE) = {match bw {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=47
  //3.2.4 Constant Generator Registers CG1 and CG2
    WORD_INSTRUCTION => Word(0x0008),
    BYTE_INSTRUCTION => Byte(0x08)
  }}

function clause read(bw, CG2, INDIRECT_AUTOINCREMENT_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=47
  //3.2.4 Constant Generator Registers CG1 and CG2
  match bw {
    WORD_INSTRUCTION => Word(0xFFFF),
    BYTE_INSTRUCTION => Byte(0xFF)
  }}

function clause read(bw, reg, INDIRECT_AUTOINCREMENT_MODE) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=55
  //3.3.6 Indirect Autoincrement Mode
  let incVal:bits(16) = match bw {
    WORD_INSTRUCTION => 0x0002,
    BYTE_INSTRUCTION => 0x0001
  };
  let Word(address) : WordByte = read(WORD_INSTRUCTION,reg,REGISTER_MODE) in {
    write(WORD_INSTRUCTION,reg,REGISTER_MODE,Word(address + incVal));
    XmemInstruction(bw,address)
  }}

/////////////////
//Register mode//
/////////////////

function clause write(BYTE_INSTRUCTION, SRCG1, REGISTER_MODE, _) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=48
  //3.2.4 Constant Generator Registers CG1 and CG2
  throw(notAllowed("Status register can not be writen by byte operations"))}


function clause write(bw, SRCG1, INDEXED_MODE, value) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=48
  //3.2.4 Constant Generator Registers CG1 and CG2
  let Word(address) : WordByte = LastInstructionFetch in
    XmemInstruction(bw,address) = value}

function clause write(bw, CG2 ,_ ,_) = {
  //texas msp430 specification document page 48????
  //Todo: See if this happens in real code
  //ex: writing the reserved bits
  //throw(undefindedBehavior("CG2 can not do write operations"))
  //TODO: this is a "problem" since it is ued by NOP instructions
  Xreg(bw,CG2) = Xreg(bw,CG2)
  }

function clause write(bw, reg, REGISTER_MODE, value) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=50
  //3.3.1 Register Mode
  Xreg(bw,reg) = value
}

////////////////
//Indexed mode//
////////////////

function clause write(bw, reg, INDEXED_MODE, value) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=51
  //3.3.2 Indexed Mode
  let Word(address) : WordByte = read(WORD_INSTRUCTION,reg,REGISTER_MODE) + LastInstructionFetch in {
    XmemInstruction(bw,address) = value}
}

function clause write(bw, reg, _, value) = {
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=49
  //3.3 Addressing Modes
  throw(notAllowed("there should be no way in the specification to write in indirect register, or absolute mode"))
}

overload X = {read, write}
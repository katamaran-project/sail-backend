//TODO :RRA RRC SWPB SXT can not have immidiate mode in the destination field -> unpredictable program operation
//manual pg 61

val rrc_inst : (BW, AM, Register) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=110
function clause rrc_inst(WORD_INSTRUCTION, AddressingMode, reg) = {
  let Word(value) : WordByte =  X(WORD_INSTRUCTION, reg, AddressingMode) in
  let shift : bits(15) = value[15 .. 1] in
  let carry :bits(1) = getCarryBit() in {
    setCarrybit(value[0 .. 0]);
    let res : WordByte = Word(carry @ shift) in {
      setResultStatusRegisters(res);
      clearOverflowbit();
      X(WORD_INSTRUCTION, reg, AddressingMode) = res
    }
  }
}

function clause rrc_inst(BYTE_INSTRUCTION, AddressingMode, reg) = {
  let Word(value) : WordByte =  X(BYTE_INSTRUCTION, reg, AddressingMode) in
  let shift : bits(7) = value[7 .. 1] in
  let carry :bits(1) = getCarryBit() in {
    setCarrybit(value[0 .. 0]);
    let res : WordByte = Byte(carry @ shift) in {
      setResultStatusRegisters(res);
      clearOverflowbit();
      X(BYTE_INSTRUCTION, reg, AddressingMode) = res
    }
  }
}

val rra_inst : (BW, AM, Register) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=109
function clause rra_inst(WORD_INSTRUCTION, AddressingMode, reg) = {
  let Word(value) : WordByte =  X(WORD_INSTRUCTION, reg, AddressingMode) in
  let shift : bits(15) = value[15 .. 1] in {
    setCarrybit(value[0 .. 0]);
    let res : WordByte = Word(value[15 .. 15] @ shift) in {
      setResultStatusRegisters(res);
      clearOverflowbit();
      X(WORD_INSTRUCTION, reg, AddressingMode) = res
    }
  }
}

function clause rra_inst(BYTE_INSTRUCTION, AddressingMode, reg) = {
  let Word(value) : WordByte =  X(BYTE_INSTRUCTION, reg, AddressingMode) in
  let shift : bits(7) = value[7 .. 1] in {
    setCarrybit(value[0 .. 0]);
    let res : WordByte = Byte(value[7 .. 7] @ shift) in {
      setResultStatusRegisters(res);
      clearOverflowbit();
      X(BYTE_INSTRUCTION, reg, AddressingMode) = res
    }
  }
}

val push_inst : (BW, AM, Register) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=101
function push_inst(bw, AddressingMode, reg) = {
  let sourceValue : WordByte =  X(bw, reg, AddressingMode) in {
    SP_reg = SP_reg + sign_extend(neg(0x2));
    writeMemInstruction(bw,SP_reg,sourceValue)
  }
}

val swpb_inst : (BW, AM, Register) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=117
function swpb_inst(_, AddressingMode, reg) = {
  let Word(value) : WordByte =  X(WORD_INSTRUCTION, reg, AddressingMode) in
  let res : WordByte = Word(value[7 .. 0] @ value[15 .. 8]) in
  X(WORD_INSTRUCTION, reg, AddressingMode) = res
}

val call_inst : (BW, AM, Register) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=73
function clause call_inst(WORD_INSTRUCTION, AddressingMode, reg) = {
  let dst : WordByte =  X(WORD_INSTRUCTION, reg, AddressingMode) in {
    SP_reg = SP_reg + sign_extend(neg(0x2));
    let PCValue = X(WORD_INSTRUCTION, PC, REGISTER_MODE) in
      writeMemInstruction(WORD_INSTRUCTION,SP_reg,PCValue);
    X(WORD_INSTRUCTION, reg, REGISTER_MODE) = dst;
  }
}

function clause call_inst(BYTE_INSTRUCTION, AddressingMode, reg) = {
  throw(notAllowed("There is no byte version of the call instruction"))
}

val reti_inst : (BW, AM, Register) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=103
function reti_inst(_, _, _) = {
  let tos : WordByte =  readMem(WORD_INSTRUCTION,SP_reg) in
    X(WORD_INSTRUCTION, SRCG1, REGISTER_MODE) = tos;
  SP_reg = SP_reg + sail_zero_extend(0x2, 16);
  let tos : WordByte =  readMem(WORD_INSTRUCTION,SP_reg) in
    X(WORD_INSTRUCTION, PC, REGISTER_MODE) = tos;
  SP_reg = SP_reg + sail_zero_extend(0x2, 16);
}



val sxt_inst : (BW, AM, Register) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=118
function sxt_inst(_, AddressingMode, reg) = {
  clearStatusRegisters();
  let Byte(value) : WordByte =  X(BYTE_INSTRUCTION, reg, AddressingMode) in
  let res : WordByte = Word(sign_extend(value)) in {
    if isNegative(res) then setNegativebit();
    if isZero(res) then setZerobit();
    if ~(isZero(res)) then setCarrybit();
    X(WORD_INSTRUCTION, reg, AddressingMode) = res
  }
}




//union clause ast = SINGLEOP : (bits(9), wordbyteInstructionBit, AddressingMode,  register)
union clause ast = SINGLEOP : (singleop, BW, AM, Register)
function clause formatAst(SINGLEOP(op, bw, As, reg)) = {
  singleopName(op) + BWstring(bw) + " " + regName(reg) + "("+ AMstring(As) +")"
}

mapping encdec_singleop : singleop <-> bits(9) = {
  RRC  <-> 0b000100000,
  RRA  <-> 0b000100010,
  PUSH <-> 0b000100100,
  SWPB <-> 0b000100001,
  CALL <-> 0b000100101,
  RETI <-> 0b000100110,
  SXT  <-> 0b000100011,
}

mapping clause encdec = SINGLEOP(op, bw, As, reg)
  <-> encdec_singleop(op) @ bitmaping(bw) @ sourcemaping(As) @ RegisterMapping(reg)

function clause execute(SINGLEOP(op, BW, AddressingMode, reg)) = {
  logWithVerbosity(2, formatAst(SINGLEOP(op, BW, AddressingMode, reg)));
  match op {
    RRC  => rrc_inst(BW, AddressingMode, reg),
    RRA  => rra_inst(BW, AddressingMode, reg),
    PUSH => push_inst(BW, AddressingMode, reg),
    SWPB => swpb_inst(BW, AddressingMode, reg),
    CALL => call_inst(BW, AddressingMode, reg),
    RETI => reti_inst(BW, AddressingMode, reg),
    SXT  => sxt_inst(BW, AddressingMode, reg),
  }
}
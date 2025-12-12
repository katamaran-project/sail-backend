//TODO :RRA RRC SWPB SXT can not have immidiate mode in the destination field -> unpredictable program operation
//manual pg 61

// https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=110
val rrc_inst : (BW, AM, Register) -> unit

function rrc_inst(bw, addressingMode, reg) = match bw {
  WORD_INSTRUCTION => {
    let Word(value) : WordByte =  X(WORD_INSTRUCTION, reg, addressingMode) in
    let shift : bits(15) = value[15 .. 1] in
    let carry : bits(1) = getCarryBit() in {
      setCarrybit(value[0 .. 0]);
      let res : WordByte = Word(carry @ shift) in {
        setResultStatusRegisters(res);
        clearOverflowbit();
        X(WORD_INSTRUCTION, reg, addressingMode) = res
      }
    }
  },

  BYTE_INSTRUCTION => {
    let Word(value) : WordByte =  X(BYTE_INSTRUCTION, reg, addressingMode) in
    let shift : bits(7) = value[7 .. 1] in
    let carry : bits(1) = getCarryBit() in {
      setCarrybit(value[0 .. 0]);
      let res : WordByte = Byte(carry @ shift) in {
        setResultStatusRegisters(res);
        clearOverflowbit();
        X(BYTE_INSTRUCTION, reg, addressingMode) = res
      }
    }
  },
}

// https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=109
val rra_inst : (BW, AM, Register) -> unit
function rra_inst(bw, am, reg) =
  let Word(value) : WordByte =  X(bw, reg, am) in
  let res : WordByte =
    match bw {
      WORD_INSTRUCTION =>
        let shift : bits(15) = value[15 .. 1] in
        { setCarrybit(value[0 .. 0]);
          let b : bits(1) = value[15 .. 15] in
          Word(b @ shift) },

      BYTE_INSTRUCTION =>
        let shift : bits(7) = value[7 .. 1] in
        { setCarrybit(value[0 .. 0]);
          let b : bits(1) = value[7 .. 7] in
          Byte(b @ shift) }
    }
  in
  { setResultStatusRegisters(res);
    clearOverflowbit();
    X(bw, reg, am) = res }

val push_inst : (BW, AM, Register) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=101
function push_inst(bw, addressingMode, reg) = {
  let sourceValue : WordByte =  X(bw, reg, addressingMode) in {
    SP_reg = SP_reg + sail_sign_extend(neg(0x2), 16);
    writeMemInstruction(bw,SP_reg,sourceValue)
  }
}

val swpb_inst : (BW, AM, Register) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=117
function swpb_inst(_, addressingMode, reg) = {
  let Word(value) : WordByte =  X(WORD_INSTRUCTION, reg, addressingMode) in
  let a : bits(8) = value[7 .. 0] in
  let b : bits(8) = value[15 .. 8] in
  let res : WordByte = Word(a @ b) in
  X(WORD_INSTRUCTION, reg, addressingMode) = res
}

// https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=73
val call_inst : (BW, AM, Register) -> unit
function call_inst(bw, addressingMode, reg) = match bw {
  WORD_INSTRUCTION => {
    let dst : WordByte =  X(WORD_INSTRUCTION, reg, addressingMode) in {
      SP_reg = SP_reg + sail_sign_extend(neg(0x2), 16);
      writeMemInstruction(WORD_INSTRUCTION, SP_reg,
                          X(WORD_INSTRUCTION, PC, REGISTER_MODE));
      X(WORD_INSTRUCTION, reg, REGISTER_MODE) = dst;
    }
  },

  BYTE_INSTRUCTION =>
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
function sxt_inst(_, addressingMode, reg) = {
  clearStatusRegisters();
  let Byte(value) : WordByte =  X(BYTE_INSTRUCTION, reg, addressingMode) in
  let res : WordByte = Word(sail_sign_extend(value, 16)) in {
    if isNegative(res) then setNegativebit();
    if isZero(res) then setZerobit();
    if ~(isZero(res)) then setCarrybit();
    X(WORD_INSTRUCTION, reg, addressingMode) = res
  }
}




//union clause ast = SINGLEOP : (bits(9), wordbyteInstructionBit, addressingMode,  register)
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

function clause execute(SINGLEOP(op, BW, addressingMode, reg)) = {
  logWithVerbosity(2, formatAst(SINGLEOP(op, BW, addressingMode, reg)));
  match op {
    RRC  => rrc_inst(BW, addressingMode, reg),
    RRA  => rra_inst(BW, addressingMode, reg),
    PUSH => push_inst(BW, addressingMode, reg),
    SWPB => swpb_inst(BW, addressingMode, reg),
    CALL => call_inst(BW, addressingMode, reg),
    RETI => reti_inst(BW, addressingMode, reg),
    SXT  => sxt_inst(BW, addressingMode, reg),
  }
}
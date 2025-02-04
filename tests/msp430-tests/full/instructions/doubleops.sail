

val move_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=97
function move_inst(x, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  let sourceValue : WordByte =  X(x, sourceRegister, AddressingModeSource) in {
  //dest value has to be got due to Addressing modes PC alignment

  LastInstructionFetch = match AddressingModeDestination {
    INDEXED_MODE => fetch(),
    _ => Word(0x0000)
  };
  X(x, destinationRegister, AddressingModeDestination) = sourceValue}
}

//these are split up to make implmenting addc like instrctions simpler
val hasCarry : (WordByte,WordByte,WordByte) -> bool
function hasCarry(w1,w2,res) = {
  unsignedWb(res) < unsignedWb(w1) + unsignedWb(w2)
}

val hasOverflowAddition : (WordByte,WordByte,WordByte) -> bool
function hasOverflowAddition(w1,w2,res) = {
  (signedWb(w1) < 0 & signedWb(w2) < 0 & signedWb(res) > 0) |
  (signedWb(w1) > 0 & signedWb(w2) > 0 & signedWb(res) < 0)
}


val addWithStatusRegister : (WordByte, WordByte) -> WordByte
function addWithStatusRegister(w1, w2) = {
  let res : WordByte = w1 + w2 in {
    if hasCarry(w1,w2,res) then setCarrybit();
    if hasOverflowAddition(w1,w2,res) then setOverflowbit();
    res
  }
}

val add_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=66
function add_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  //addition will set the needed status bits
  clearStatusRegisters();
  let sourceValue : WordByte = X(bw, sourceRegister , AddressingModeSource) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in
  let result : WordByte = addWithStatusRegister(sourceValue,destValue) in {
    setResultStatusRegisters(result);
    X(bw, destinationRegister, AddressingModeDestination) = result
  }
}

val addc_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=67
function addc_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  let sourceValue : WordByte = X(bw, sourceRegister , AddressingModeSource) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in
  let carry : WordByte = BW_zero_extend(getCarryBit()) in {
    //addition will set the needed status bits
    clearStatusRegisters();
    let result : WordByte = addWithStatusRegister(addWithStatusRegister(sourceValue,destValue),carry) in {
      setResultStatusRegisters(result);
      X(bw, destinationRegister, AddressingModeDestination) = result
    }
  }
}


val sub_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=115
function sub_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  //addition will set the needed status bits
  clearStatusRegisters();
  let sourceValue : WordByte = ~(X(bw, sourceRegister , AddressingModeSource)) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in
  let result : WordByte = addWithStatusRegister(addWithStatusRegister(sourceValue,destValue), BW_zero_extend(0x1)) in {
    setResultStatusRegisters(result);
    X(bw, destinationRegister, AddressingModeDestination) = result
  }
}

val subc_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=116
function subc_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {

  let sourceValue : WordByte = ~(X(bw, sourceRegister , AddressingModeSource)) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in
  let carry : WordByte = BW_zero_extend(getCarryBit()) in {
    //addition will set the needed status bits
    clearStatusRegisters();
    let result : WordByte = addWithStatusRegister(addWithStatusRegister(sourceValue,destValue), carry) in {
    setResultStatusRegisters(result);
    X(bw, destinationRegister, AddressingModeDestination) = result
  }
  }
}

val cmp_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=78
function cmp_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  //addition will set the needed status bits
  clearStatusRegisters();
  let sourceValue : WordByte = ~(X(bw, sourceRegister , AddressingModeSource)) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in
  let result : WordByte   = addWithStatusRegister(addWithStatusRegister(sourceValue,destValue), BW_zero_extend(0x1)) in
    setResultStatusRegisters(result);
}

val andWithStatusRegister : (WordByte, WordByte) -> WordByte
function andWithStatusRegister(w1, w2) = {
  let res : WordByte = w1 & w2 in {
    if isNegative(res) then setNegativebit();
    if isZero(res) then setZerobit();
    if ~(isZero(res)) then setCarrybit();
    res
  }
}


val and_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=68
function and_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  clearStatusRegisters();
  let sourceValue : WordByte = X(bw, sourceRegister , AddressingModeSource) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in
  let result : WordByte = andWithStatusRegister(sourceValue,destValue) in {
    X(bw, destinationRegister, AddressingModeDestination) = result
  }
}

val xorWithStatusRegister : (WordByte, WordByte) -> WordByte
function xorWithStatusRegister(w1, w2) = {
  let res : WordByte = w1 ^ w2 in {
    if isNegative(res) then setNegativebit();
    if isZero(res) then setZerobit();
    if ~(isZero(res)) then setCarrybit();
    if isNegative(w1) & isNegative(w2) then setOverflowbit();
    res
  }
}


val xor_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=120
function xor_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  clearStatusRegisters();
  let sourceValue : WordByte = X(bw, sourceRegister , AddressingModeSource) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in
  let result : WordByte = xorWithStatusRegister(sourceValue,destValue) in {
    X(bw, destinationRegister, AddressingModeDestination) = result
  }
}

val bic_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=69
function bic_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  let sourceValue : WordByte = ~(X(bw, sourceRegister , AddressingModeSource)) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in
  let result : WordByte = sourceValue & destValue in {
    X(bw, destinationRegister, AddressingModeDestination) = result
  }
}

val bis_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=70
function bis_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  let sourceValue : WordByte = X(bw, sourceRegister , AddressingModeSource) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in
  let result : WordByte = sourceValue | destValue in {
    X(bw, destinationRegister, AddressingModeDestination) = result
  }
}

val bit_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=71
function bit_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  let sourceValue : WordByte = X(bw, sourceRegister , AddressingModeSource) in
  let destValue : WordByte   = X(bw, destinationRegister , AddressingModeDestination) in {
    //only changes the status register no writing needed
    let _ : WordByte   = andWithStatusRegister(sourceValue,destValue);
  }
}


val asDecimal : (WordByte) -> int
function clause asDecimal(Word(a:bits(4) @ b:bits(4) @ c:bits(4) @ d:bits(4))) = {
  unsigned(a) * 1000 + unsigned(b) * 100 + unsigned(c) * 10 + unsigned(d)
}
function clause asDecimal(Byte(a:bits(4) @ b:bits(4))) = {
  unsigned(a) * 10 + unsigned(b)
}

val asWordByte : int -> (WordByte)
function asWordByte(number) = {
  Word(get_slice_int(4,(number/1000),0) : bits(4) @ get_slice_int(4,(number%1000/100),0) : bits(4) @ get_slice_int(4,(number%100/10),0) : bits(4) @ get_slice_int(4,(number%10),0) : bits(4))
}

val dadd_inst : (BW, Register, AM, Register, AM) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=80
function dadd_inst(bw, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination) = {
  let sourceValue : int = asDecimal(X(bw, sourceRegister , AddressingModeSource)) in
  let destValue : int   = asDecimal(X(bw, destinationRegister , AddressingModeDestination)) in {
    //only changes the status register no writing needed
    let res : WordByte   = asWordByte(sourceValue + destValue) in {
      if isNegative(res) then setNegativebit();
      if isZero(res) then setZerobit();
      match bw {
       BYTE_INSTRUCTION => if sourceValue + destValue > 99 then setCarrybit(),
       WORD_INSTRUCTION => if sourceValue + destValue > 9999 then setCarrybit()
      }
    };
  }
}




//union clause ast = DOUBLEOP : (bits(4), wordbyteInstructionBit, registerAddress, AddressingModeSource, registerAddress, AddressingModeDestination)
union clause ast = DOUBLEOP : (doubleop, BW, Register, AM, Register, AM)
val formatAst: (ast) -> string
function clause formatAst(DOUBLEOP(op, bw, sourceReg, As, destinationReg, Ad)) = {
  duopName(op) + BWstring(bw) + " " + regName(sourceReg) + "("+ AMstring(As) +")" + regName(destinationReg) //+ "(" + AMstring(Ad) +")"
                                                                                                            //Todo: adding this adds like a minute of compile time
}

mapping encdec_doubleop : doubleop <-> bits(4) = {
  MOV  <-> 0x4,
  ADD  <-> 0x5,
  ADDC <-> 0x6,
  SUB  <-> 0x7,
  SUBC <-> 0x8,
  CMP  <-> 0x9,
  DADD <-> 0xA,
  BIT  <-> 0xB,
  BIC  <-> 0xC,
  BIS  <-> 0xD,
  XOR  <-> 0xE,
  AND  <-> 0xF,
}

mapping clause encdec = DOUBLEOP(op, bw, sourceReg, As, destinationReg, Ad)
  <-> encdec_doubleop(op) @ RegisterMapping(sourceReg) @ destinationmaping(Ad) @ bitmaping(bw) @ sourcemaping(As) @ RegisterMapping(destinationReg)

function clause execute(DOUBLEOP(op, BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination)) = {
  logWithVerbosity(2, formatAst(DOUBLEOP(op, BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination)));
  match op {
    MOV  => move_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    ADD  => add_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    ADDC => addc_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    SUB  => sub_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    SUBC => subc_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    CMP  => cmp_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    DADD => dadd_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    BIT  => bit_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    BIC  => bic_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    BIS  => bis_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    XOR  => xor_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
    AND  => and_inst(BW, sourceRegister, AddressingModeSource, destinationRegister, AddressingModeDestination),
  }
}




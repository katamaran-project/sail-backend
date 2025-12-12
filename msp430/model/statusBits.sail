//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=47
//3.2.3 Status Register (SR)


// V
val getOverflowBit : (unit) -> bits(1)
function getOverflowBit() = SRCG1_reg[8 .. 8]

val setOverflowbitBit: (bits(1)) -> unit
function setOverflowbitBit(b) = {SRCG1_reg[8 .. 8] = b}
val setOverflowbitTrue: unit -> unit
function setOverflowbitTrue() = {SRCG1_reg[8 .. 8] = 0b1}
overload setOverflowbit = {setOverflowbitBit, setOverflowbitTrue}

val clearOverflowbit: unit -> unit
function clearOverflowbit() = {SRCG1_reg[8 .. 8] = 0b0}

val overflowbitSet: unit -> bool
function overflowbitSet() = {SRCG1_reg[8 .. 8] == 0b1}

// N
val getNegativeBit : (unit) -> bits(1)
function getNegativeBit() = SRCG1_reg[2 .. 2]

val setNegativebitBit: (bits(1)) -> unit
function setNegativebitBit(b) = {SRCG1_reg[2 .. 2] = b}
val setNegativebitTrue: unit -> unit
function setNegativebitTrue() = {SRCG1_reg[2 .. 2] = 0b1}
overload setNegativebit = {setNegativebitBit, setNegativebitTrue}

val clearNegativebit: unit -> unit
function clearNegativebit() = {SRCG1_reg[2 .. 2] = 0b0}

val negativebitSet: unit -> bool
function negativebitSet() = {SRCG1_reg[2 .. 2] == 0b1}

// Z
val getZeroBit : (unit) -> bits(1)
function getZeroBit() = SRCG1_reg[1 .. 1]

val setZerobitBit: (bits(1)) -> unit
function setZerobitBit(b) = {SRCG1_reg[1 .. 1] = b}
val setZerobitTrue: unit -> unit
function setZerobitTrue() = {SRCG1_reg[1 .. 1] = 0b1}
overload setZerobit = {setZerobitBit, setZerobitTrue}

val clearZerobit: unit -> unit
function clearZerobit() = {SRCG1_reg[1 .. 1] = 0b0}

val zerobitSet: unit -> bool
function zerobitSet() = {SRCG1_reg[1 .. 1] == 0b1}

// C
val getCarryBit : (unit) -> bits(1)
function getCarryBit() = SRCG1_reg[0 .. 0]

val setCarrybitBit: (bits(1)) -> unit
function setCarrybitBit(b) = {SRCG1_reg[0 .. 0] = b}
val setCarrybitTrue: unit -> unit
function setCarrybitTrue() = {SRCG1_reg[0 .. 0] = 0b1}
overload setCarrybit = {setCarrybitBit, setCarrybitTrue}

val clearCarrybit: unit -> unit
function clearCarrybit() = {SRCG1_reg[0 .. 0] = 0b0}

val carrybitSet: unit -> bool
function carrybitSet() = {SRCG1_reg[0 .. 0] == 0b1}

//mainly used for testing
val setAllStatusbits : (unit) -> unit
function setAllStatusbits() = {
  setCarrybit();
  setNegativebit();
  setZerobit();
  setOverflowbit();
}

//instructions that efect all registers can use this
val clearStatusRegisters: (unit) -> unit
function clearStatusRegisters() = {
  clearNegativebit();
  clearCarrybit();
  clearOverflowbit();
  clearZerobit();
}

//many instructions do adaptations depending on the value written to the destination
//this exists to be a shorthand for that opperation
val setResultStatusRegisters : (WordByte) -> unit
function setResultStatusRegisters(res)= {
  if isNegative(res) then setNegativebit();
  if isZero(res) then setZerobit();
}
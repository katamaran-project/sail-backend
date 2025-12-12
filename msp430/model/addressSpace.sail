val writeReg : (BW, Register, WordByte) -> unit


// used only to implement readMem/writeMem that check permissions
val read_ram = pure "read_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m)) -> bits(8 * 'n)
val write_ram = pure "write_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m), bits(8 * 'n)) -> unit

val read_mem_aux : (BW, Address) -> WordByte

function read_mem_aux(bw, addr) = match bw {
  BYTE_INSTRUCTION => {
    check_byte_access(addr, false);
    Byte(if is_mpu_reg_addr(addr)
         then read_mpu_reg_byte(addr)
         else read_ram(sizeof(address_size), 1, 0x0000, addr))
  },

  WORD_INSTRUCTION =>
    let addr = addr & 0xFFFE in
    let Byte(l) = read_mem_aux(BYTE_INSTRUCTION, addr) in
    let Byte(h) = read_mem_aux(BYTE_INSTRUCTION, addr | 0x0001) in
    Word(h @ l)
}

val readMem : (BW, Address) -> WordByte
function readMem(bw, addr) = read_mem_aux(bw, addr)

val writeMem : (BW, Address, WordByte) -> unit

function writeMem(bw, addr, v) = match bw {
  BYTE_INSTRUCTION => match v {
    Byte(v) => {
      check_byte_access(addr, false);
      if is_mpu_reg_addr(addr)
      then write_mpu_reg_byte(addr, v)
      else write_ram(sizeof(address_size), 1, 0x0000, addr, v)
    },

    // high bytes go unused when writing byte operations to ram
    // https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=48
    // 3.2.5 General-Purpose Registers R4 to R15
    Word(v) => writeMem(BYTE_INSTRUCTION, addr, Byte(v[7 .. 0]))
  },

  WORD_INSTRUCTION => match v {
    Word(v) => {
      let addr = addr & 0xFFFE in {
        writeMem(BYTE_INSTRUCTION, addr + 1, Byte(v[15 .. 8]));
        writeMem(BYTE_INSTRUCTION, addr, Byte(v[7 .. 0]))
      }
    },

    Byte(v) => writeMem(WORD_INSTRUCTION, addr, Word(sail_zero_extend(v, 16)))
  }
}

overload Xmem = {readMem, writeMem}

//separated for logging
val readMemInstruction: (BW, Address) -> WordByte
function clause readMemInstruction(bw, address) = {
  let res = Xmem(bw, address) in {
    logWithVerbosity(4,"memory:" + bits_str(old_PC_reg) + "   -GET: [" + bits_str(address) + "]" + BWSizeString(bw) + "->" + WordByteString(res));
    res
  }
}

val writeMemInstruction: (BW, Address, WordByte) -> unit
function clause writeMemInstruction(bw, address, value) = {
  logWithVerbosity(4,"memory:" + bits_str(old_PC_reg) + "   -PUT: [" + bits_str(address) + "]" + BWSizeString(bw) + "<-" + WordByteString(value));
  Xmem(bw,address) = value
}

overload XmemInstruction = {readMemInstruction, writeMemInstruction}

// PC should always be word aligned
val incPC: unit -> unit
function incPC() = writeReg(WORD_INSTRUCTION, PC, Word(PC_reg + 0x0002))

val setPC: wordBits -> unit
function setPC(v) = writeReg(WORD_INSTRUCTION, PC, Word(v))

// fetch is used for 2 things:
// - reading the instructions
// - reading values for certain addressing modes
val fetch: unit -> WordByte
function fetch() = {
  // PC should always be word aligned
  // https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=45
  // 3.2.1 Program Counter (PC)

  // TODO maybe do this check at eatch FDE step so we can also do this for the stack pointer
  assert(PC_reg[0..0] == 0b0, "PC is not correctly aligned");

  let data = read_mem_aux(WORD_INSTRUCTION, PC_reg);
  incPC();
  data
}

/* [not used]

// readPC can be given an offset
// such that we can read the value for the prevous fetch for example
val readPCWithOffset: wordBits -> WordByte
function readPCWithOffset(offset) = {
  Xmem(WORD_INSTRUCTION, PC_reg + offset)
}
val readPCWithNoOffset: unit -> WordByte
function readPCWithNoOffset() = {
  Xmem(WORD_INSTRUCTION, PC_reg)
}

overload  readPC = {readPCWithNoOffset, readPCWithOffset}

*/
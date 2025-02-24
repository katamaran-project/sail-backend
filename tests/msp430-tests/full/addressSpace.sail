
// functions to do random acces memory
val write_ram = pure "write_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m), bits(8 * 'n)) -> unit

val read_ram = pure "read_ram" : forall 'n 'm.
  (atom('m), atom('n), bits('m), bits('m)) -> bits(8 * 'n)




val readMem: (BW, Address) -> WordByte
function clause readMem(WORD_INSTRUCTION, address) = {
  let value = read_ram(sizeof(address_size),2,sizeof(address_size)^0x0,address) in
  Word(value)
}
function clause readMem(BYTE_INSTRUCTION, address) = {
  let Word(value) : WordByte = readMem(WORD_INSTRUCTION, address) in
  Byte(value[7 .. 0])
}

val writeMem: (BW, Address, WordByte) -> unit
function clause writeMem(WORD_INSTRUCTION, address, Word(wb)) = {
  write_ram(sizeof(address_size),2,sizeof(address_size)^0x0,address,wb)
}

//high bytes go unused when writing byte operations to ram
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=48
//3.2.5 General-Purpose Registers R4 to R15
function clause writeMem(BYTE_INSTRUCTION, address, Word(wbTolong)) = {
  let wb:bits(8) = wbTolong[7..0] in
  write_ram(sizeof(address_size),1,sizeof(address_size)^0x0,address,wb)
}

function clause writeMem(bw, address, Byte(bb)) = {
    writeMem(bw, address, Word(sail_zero_extend(bb, 16)))
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


//PC should always be word aligned
val incPC: unit -> unit
function incPC() = {
  PC_reg = PC_reg + 0x0002;
}

//fetch is used for 2 things:
// -reading the instructions
// -reading values for certain Addressing modes
val fetch: unit -> WordByte
function fetch() = {
  //PC should always be word aligned
  //https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=45
  //3.2.1 Program Counter (PC)

  //TODO maybe du this check at eatch FDE step so we can also do this for the stack pointer


  assert(unsigned(PC_reg)%2 == 0,"PC is not correctly aligned");
  let data = Xmem(WORD_INSTRUCTION, PC_reg);
  incPC();
  data
}

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
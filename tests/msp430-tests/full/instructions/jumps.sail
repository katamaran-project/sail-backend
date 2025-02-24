type OffsetLen : Int = 10
type Offset = bits(OffsetLen)

union clause ast = JUMP : (jump, bits(10))


val jeq_inst : (Offset) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=90
function jeq_inst(offset) = {
  if(zerobitSet()) then PC_reg = PC_reg + sail_sign_extend(offset << 1, 16)
}

val jne_inst : (Offset) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=96
function jne_inst(offset) = {
  if(~(zerobitSet())) then PC_reg = PC_reg + sail_sign_extend(offset << 1, 16)
}

val jc_inst : (Offset) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=89
function jc_inst(offset) = {
  if(carrybitSet()) then PC_reg = PC_reg + sail_sign_extend(offset << 1, 16)
}

val jnc_inst : (Offset) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=95
function jnc_inst(offset) = {
  if(~(carrybitSet())) then PC_reg = PC_reg + sail_sign_extend(offset << 1, 16)
}

val jn_inst : (Offset) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=94
function jn_inst(offset) = {
  if(negativebitSet()) then PC_reg = PC_reg + sail_sign_extend(offset << 1, 16)
}

val jge_inst : (Offset) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=91
function jge_inst(offset) = {
  if(~(overflowbitSet() ^ negativebitSet())) then PC_reg = PC_reg + sail_sign_extend(offset << 1, 16)
}

val jl_inst : (Offset) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=92
function jl_inst(offset) = {
  if(overflowbitSet() ^ negativebitSet()) then PC_reg = PC_reg + sail_sign_extend(offset << 1, 16)
}

val jmp_inst : (Offset) -> unit
//https://www.ti.com/lit/ug/slau144k/slau144k.pdf#page=93
function jmp_inst(offset) = {
  PC_reg = PC_reg + sail_sign_extend(offset << 1, 16)
}

mapping encdec_jump : jump <-> bits(3) = {
  JEQ <-> 0b001,
  JNE <-> 0b000,
  JC  <-> 0b011,
  JNC <-> 0b010,
  JN  <-> 0b100,
  JGE <-> 0b101,
  JL  <-> 0b110,
  JMP <-> 0b111,
}

mapping clause encdec = JUMP(op, offset)
  <-> 0b001 @ encdec_jump(op) @ offset:Offset

function clause formatAst(JUMP(op, offset)) = {
  jumpName(op) + " " + bits_str(offset)
}


function clause execute(JUMP(op, offset)) = {
  logWithVerbosity(2, formatAst(JUMP(op, offset)));
  match op {
    JEQ => jeq_inst(offset),
    JNE => jne_inst(offset),
    JC  => jc_inst(offset),
    JNC => jnc_inst(offset),
    JN  => jn_inst(offset),
    JGE => jge_inst(offset),
    JL  => jl_inst(offset),
    JMP => jmp_inst(offset),
  }
}

type regname = bits(2)
type dst     = regname
type src     = regname
type imm     = bits(12)
type imm_ext = bits(20)
let imm_size = 12
let imm_ext_size = 20

scattered union ast
union clause ast = JALR_CAP      : (dst, src)
union clause ast = CJALR         : (dst, src, imm)
union clause ast = CJAL	         : (dst, imm_ext)
union clause ast = BNE           : (src, src, imm)
union clause ast = LD            : (dst, src, imm)
union clause ast = SD            : (src, src, imm)
union clause ast = ADDI          : (dst, src, imm)
union clause ast = ADD           : (dst, src, src)
union clause ast = SUB           : (dst, src, src)
union clause ast = SLT           : (dst, src, src)
union clause ast = SLTI          : (dst, src, imm)
union clause ast = SLTIU         : (dst, src, imm)
union clause ast = SLTU          : (dst, src, src)
union clause ast = CMOVE         : (dst, src)
union clause ast = CINCOFFSET    : (dst, src, src)
union clause ast = CANDPERM      : (dst, src, src)
union clause ast = CSETBOUNDS    : (dst, src, src)
union clause ast = CSETBOUNDSIMM : (dst, src, imm)
union clause ast = CGETTAG       : (dst, src)
union clause ast = CGETPERM      : (dst, src)
union clause ast = CGETBASE      : (dst, src)
union clause ast = CGETLEN       : (dst, src)
union clause ast = CGETADDR      : (dst, src)
union clause ast = FAIL          : unit
union clause ast = RET           : unit

val writeReg : (regname, word) -> unit
function writeReg(rs, w) = {
  match rs {
    0b00 => R0 = w,
    0b01 => R1 = w,
    0b10 => R2 = w,
    0b11 => R3 = w
  }
  // let i = unsigned(rs) in
  // if i != 0
  // then (*registers[i]) = w
}

val readReg : src -> word
function readReg(rs) = {
  match rs {
    0b00 => Num(sail_zeros(integer_size)),
    0b01 => R1,
    0b10 => R2,
    0b11 => R3
  }
  // let i = unsigned(rs) in
  // if i == 0
  // then Num(sail_zeros(integer_size))
  // else *(registers[i])
}

val readRegCap : src -> Capability
function readRegCap(cs) = {
  let w = readReg(cs) in
  match w {
    Cap(c) => c,
    _ => throw(ExpectedCapabilityRegisterContents())
  }
}

val readRegNum : src -> integer
function readRegNum(rs) = {
  let w = readReg(rs) in
  match w {
    Num(i) => i,
    _ => throw(ExpectedNumberRegisterContents())
  }
}

function isPerm(p, p') : (Permission, Permission) -> bool =
  match p {
    O => match p' {
      	   O => true,
	   _ => false
	 },
    R => match p' {
           R => true,
           _ => false
	 },
    RW => match p' {
            RW => true,
            _ => false
	  },
    E => match p' {
           E => true,
           _ => false
	 }
  }

function isNotZero(i) : imm -> bool =
  i != sail_zeros(imm_size)

function canIncrCursor(c, imm) : (Capability, imm) -> bool =
  not(isPerm(c.cap_permission, E)) | not(isNotZero(imm))

function updatePCPerm(c) : Capability -> Capability =
  match c.cap_permission {
    E => {c with cap_permission = R },
    _ => c
  }

val execute : ast -> bool
scattered function execute

function clause execute(JALR_CAP(cd, cs)) =
  execute(CJALR(cd, cs, sail_zeros(imm_size)))

function clause execute(CJALR(cd, cs, imm)) = {
  let npc = nextPC();
  writeReg(cd) = Cap(npc);
  let c = readRegCap(cs);
  if not(canIncrCursor(c, imm))
  then throw(CapabilityCursorCannotBeModified())
  else {
    let imm = sail_zero_extend(imm, integer_size);
    let c' = updatePCPerm({c with cap_cursor = c.cap_cursor + imm});
    PC = c';
    true
  }
}

function clause execute(CJAL(cd, imm)) = {
  let npc = nextPC();
  writeReg(cd) = Cap(npc);
  addPC(sail_shiftleft(sail_zero_extend(imm, integer_size), 1));
  true
}

function clause execute(BNE(rs1, rs2, imm)) = {
  let a = readRegNum(rs1);
  let b = readRegNum(rs2);
  if a == b
  then updatePC()
  else addPC(sail_zero_extend(imm, integer_size));
  true
}

function clause execute(CMOVE(cd, cs)) = {
  writeReg(cd) = readReg(cs);
  updatePC();
  true
}

function clause execute(CGETTAG(rd, cs)) = {
  let w = readReg(cs);
  match w {
    Num(_) => writeReg(rd) = Num(sail_zero_extend(0b0, integer_size)),
    Cap(_) => writeReg(rd) = Num(sail_zero_extend(0b1, integer_size))
  };
  updatePC();
  true
}

function clause execute(CGETPERM(rd, cs)) = {
  let c = readRegCap(cs);
  let i = sail_zero_extend(perm_bits(c.cap_permission), integer_size);
  writeReg(rd) = Num(i);
  updatePC();
  true
}

function clause execute(CGETBASE(rd, cs)) = {
  let c = readRegCap(cs);
  writeReg(rd) = Num(c.cap_begin);
  updatePC();
  true
}

function clause execute(CGETLEN(rd, cs)) = {
  let c = readRegCap(cs);
  let res = sub_bits(c.cap_end, c.cap_begin); // TODO: is sub_bits correct here?
  writeReg(rd) = Num(res);
  updatePC();
  true
}

function clause execute(CGETADDR(rd, cs)) = {
  let c = readRegCap(cs);
  writeReg(rd) = Num(c.cap_cursor);
  updatePC();
  true
}

function clause execute(SD(rs1, rs2, imm)) = {
  let base_cap = readRegCap(rs1);
  let c = {base_cap with cap_cursor = base_cap.cap_cursor + sail_sign_extend(imm, integer_size) };
  assert(writeAllowed(c.cap_permission), "Err: [store] no read permission");
  let w = readReg(rs2);
  write_mem(c, w);
  updatePC();
  true
}

function clause execute(LD(cd, cs, imm)) = {
  let base_cap = readRegCap(cs);
  let c = {base_cap with cap_cursor = base_cap.cap_cursor + sail_sign_extend(imm, integer_size) };
  assert(readAllowed(c.cap_permission), "Err: [load] no read permission");
  let n = read_mem(c);
  writeReg(cd) = n;
  updatePC();
  true
}

function clause execute(CINCOFFSET(cd, cs, rs)) = {
  let base_cap = readRegCap(cs);
  let offset = readRegNum(rs);
  match base_cap.cap_permission {
    E => throw(CINCOFFSETOnEnterCapability()),
    _ => {
        let c = { base_cap with cap_cursor = base_cap.cap_cursor + offset };
	writeReg(cd) = Cap(c);
	updatePC();
	true
      }
  }
}

function clause execute(CANDPERM(cd, cs, rs)) = {
  let cs_val = readRegCap(cs);
  let rs_val = readRegNum(rs);
  let p = perm_bits(cs_val.cap_permission);
  let p' = rs_val[2..0];
  let new_p = perm_bits(p & p');
  let new_cap = {cs_val with cap_permission = new_p };
  writeReg(cd) = Cap(new_cap);
  updatePC();
  true
}

function clause execute(ADDI(rd, rs, imm)) = {
  let v = readRegNum(rs);
  let res = v + sail_sign_extend(imm, integer_size);
  writeReg(rd) = Num(res);
  updatePC();
  true
}

function clause execute(ADD(rd, rs1, rs2)) = {
  let v1 = readRegNum(rs1);
  let v2 = readRegNum(rs2);
  let res = v1 + v2;
  writeReg(rd) = Num(res);
  updatePC();
  true
}

function clause execute(SUB(rd, rs1, rs2)) = {
  let v1 = readRegNum(rs1);
  let v2 = readRegNum(rs2);
  let res = signed(v1) - signed(v2);
  writeReg(rd) = Num(get_slice_int(integer_size, res, 0));
  updatePC();
  true
}

function clause execute(SLT(rd, rs1, rs2)) = {
  let v1 = readRegNum(rs1);
  let v2 = readRegNum(rs2);
  if signed(v1) < signed(v2) then
    writeReg(rd) = Num(sail_zero_extend(0b1, integer_size))
  else
    writeReg(rd) = Num(sail_zero_extend(0b0, integer_size));
  updatePC();
  true
}

function clause execute(SLTI(rd, rs, imm)) = {
  let v1 = readRegNum(rs);
  let v2 = sail_sign_extend(imm, integer_size);
  if signed(v1) < signed(v2) then
    writeReg(rd) = Num(sail_zero_extend(0b1, integer_size))
  else
    writeReg(rd) = Num(sail_zero_extend(0b0, integer_size));
  updatePC();
  true
}

function clause execute(SLTU(rd, rs, rb)) = {
  let v1 = readRegNum(rs);
  let uv1 = unsigned(v1);
  let v2 = readRegNum(rb);
  let uv2 = unsigned(v2);
  if uv1 < uv2 then
    writeReg(rd) = Num(sail_zero_extend(0b1, integer_size))
  else
    writeReg(rd) = Num(sail_zero_extend(0b0, integer_size));
  updatePC();
  true
}

function clause execute(SLTIU(rd, rs, immediate)) = {
  let v1 = readRegNum(rs);
  let uv1 = unsigned(v1);
  let v2 = sail_sign_extend(immediate, integer_size);
  let uv2 = unsigned(v2);
  if uv1 < uv2 then
    writeReg(rd) = Num(sail_zero_extend(0b1, integer_size))
  else
    writeReg(rd) = Num(sail_zero_extend(0b0, integer_size));
  updatePC();
  true
}

function clause execute(CSETBOUNDS(cd, cs, rs)) = {
  let c = readRegCap(cs);
  let new_begin = c.cap_cursor;
  let rs_val = readRegNum(rs);
  let new_end = new_begin + rs_val;
  match c.cap_permission {
    E => throw(CSETBOUNDSOnEnterCapability()),
    _ => {
      let b = isWithinRange(new_begin, new_end, c.cap_begin, c.cap_end);
      assert(b, "Err: [csetbounds] tried to increase range of authority");
      let c' = { c with cap_begin = new_begin, cap_end = new_end };
      writeReg(cd) = Cap(c');
      updatePC();
      true
    }
  }
}

function clause execute(CSETBOUNDSIMM(cd, cs, imm)) = {
  let c = readRegCap(cs);
  let new_begin = c.cap_cursor;
  let new_end = new_begin + sail_zero_extend(imm, integer_size);
  match c.cap_permission {
    E => throw(CSETBOUNDSOnEnterCapability()),
    _ => {
      let b = isWithinRange(new_begin, new_end, c.cap_begin, c.cap_end);
      assert(b, "Err: [csetbounds] tried to increase range of authority");
      let c' = { c with cap_begin = new_begin, cap_end = new_end };
      writeReg(cd) = Cap(c');
      updatePC();
      true
    }
  }
}

function clause execute(RET()) = false

function clause execute(FAIL()) = throw(Fail())

// translate RISC-V Register to Minimal Caps Register
val tREG : bits(5) -> regname
function tREG(rs) = {
  match rs {
    0b01010 => 0b00,
    0b01011 => 0b01,
    0b01100 => 0b10,
    0b01101 => 0b11,
    _ => 0b00
  }
}

val decode : instruction -> option(ast)
scattered function decode

function clause decode (0b000000000000                @ 0b00001      @ 0b000 @ 0b00000        @ 0b1100111)
  = Some(RET())
function clause decode (0b000000000000                @ 0b00000      @ 0b000 @ 0b00000        @ 0b1100111)
  = Some(FAIL())
function clause decode(0b1111111 @ 0b01100 @ cs : bits(5) @ 0b000 @ cd : bits(5) @ 0b1011011)
  = Some(JALR_CAP(tREG(cd), tREG(cs)))
function clause decode(imm : imm @ cs : bits(5) @ 0b000 @ cd : bits(5) @ 0b1100111)
  = Some(CJALR(tREG(cd), tREG(cs), imm))
function clause decode(imm_19 : bits(1) @ imm_18_13 : bits(6) @ imm_12_9 : bits(4) @ imm_8 : bits(1) @ imm_7_0 : bits(8) @ cd : bits(5) @ 0b1101111)
  = Some(CJAL(tREG(cd), imm_19 @ imm_7_0 @ imm_8 @ imm_18_13 @ imm_12_9))
function clause decode(imm7_6 : bits(1) @ imm7_5_0 : bits(6) @ rs2 : bits(5) @ rs1 : bits(5) @ 0b001 @ imm5_4_1 : bits(4) @ imm5_0 : bits(1) @ 0b1100011)
  = Some(BNE(tREG(rs1), tREG(rs2), imm7_6 @ imm5_0 @ imm7_5_0 @ imm5_4_1))
function clause decode(0b1111111 @ 0b01010 @ cs : bits(5) @ 0b000 @ cd : bits(5) @ 0b1011011)
  = Some(CMOVE(tREG(cd), tREG(cs)))
function clause decode(imm : imm               @ rs : bits(5) @ 0b011 @ rd : bits(5)   @ 0b0000011)
  = Some(LD(tREG(rd), tREG(rs), imm))
function clause decode(imm1 : bits(7) @ rs : bits(5) @ rb : bits(5) @ 0b011 @ imm2 : bits(5) @ 0b0100011)
  = Some(SD(tREG(rs), tREG(rb), append(imm1, imm2)))
function clause decode(imm : imm@ rs : bits(5) @ 0b000 @ rd : bits(5)   @ 0b0010011)
  = Some(ADDI(tREG(rd), tREG(rs), imm))
function clause decode(0b0000000 @ rb : bits(5)      @ rs : bits(5) @ 0b000 @ rd : bits(5)   @ 0b0110011)
  = Some(ADD(tREG(rd), tREG(rs), tREG(rb)))
function clause decode(0b0100000 @ rb : bits(5)      @ rs : bits(5) @ 0b000 @ rd : bits(5)   @ 0b0110011)
  = Some(SUB(tREG(rd), tREG(rs), tREG(rb)))
function clause decode(0b0100000 @ 0b00000           @ rs : bits(5) @ 0b101 @ rd : bits(5)   @ 0b0110011)
  = Some(CGETPERM(tREG(rd), tREG(rs)))
function clause decode(0b0100000 @ 0b00001           @ rs : bits(5) @ 0b101 @ rd : bits(5)   @ 0b0110011)
  = Some(CGETBASE(tREG(rd), tREG(rs)))
function clause decode(0b0100000 @ 0b00010           @ rs : bits(5) @ 0b101 @ rd : bits(5)   @ 0b0110011)
  = Some(CGETLEN(tREG(rd), tREG(rs)))
function clause decode(0b0100000 @ 0b00011           @ rs : bits(5) @ 0b101 @ rd : bits(5)   @ 0b0110011)
  = Some(CGETADDR(tREG(rd), tREG(rs)))
function clause decode(0b0010001 @ rs : bits(5) @ cs : bits(5) @ 0b000 @ cd : bits(5) @ 0b1011011)
  = Some(CINCOFFSET(tREG(cd), tREG(cs), tREG(rs)))
function clause decode(0b0100000 @ 0b00101           @ rs : bits(5) @ 0b101 @ rd : bits(5)   @ 0b0110011)
  = Some(CGETTAG(tREG(rd), tREG(rs)))
function clause decode(0b0001101 @ rs : bits(5) @ cs : bits(5) @ 0b000 @ cd : bits(5) @ 0b1011011)
  = Some(CANDPERM(tREG(cd), tREG(cs), tREG(rs)))
function clause decode (0b0000000 @ rb : bits(5)      @ rs : bits(5) @ 0b010 @ rd : bits(5)   @ 0b0110011)
  = Some(SLT(tREG(rd), tREG(rs), tREG(rb)))
function clause decode (imm : imm               @ rs : bits(5) @ 0b010 @ rd : bits(5)   @ 0b0010011)
  = Some(SLTI(tREG(rd), tREG(rs), imm))
function clause decode (0b0000000 @ rb : bits(5)      @ rs : bits(5) @ 0b011 @ rd : bits(5)   @ 0b0110011)
  = Some(SLTU(tREG(rd), tREG(rs), tREG(rb)))
function clause decode (imm : imm               @ rs : bits(5) @ 0b011 @ rd : bits(5)   @ 0b0010011)
  = Some(SLTIU(tREG(rd), tREG(rs), imm))
function clause decode(0b0001000 @ rs : bits(5) @ cs : bits(5) @ 0b000 @ cd : bits(5) @ 0b1011011)
  = Some(CSETBOUNDS(tREG(cd), tREG(cs), tREG(rs)))
function clause decode(imm : imm @ cs : bits(5) @ 0b010 @ cd : bits(5) @ 0b1011011)
  = Some(CSETBOUNDSIMM(tREG(cd), tREG(cs), imm))
function clause decode (not_supported)
  = None()
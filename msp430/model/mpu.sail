enum mpu_register_name = {
  MPUCTL0,
  MPUCTL1,
  MPUSEGB2,
  MPUSEGB1,
  MPUSAM,
  MPUIPC0,
  MPUIPSEGB2,
  MPUIPSEGB1,
}
mapping mpu_register_index : mpu_register_name <-> range(0, 7) = {
  MPUCTL0 <-> 0,
  MPUCTL1 <-> 1,
  MPUSEGB2 <-> 2,
  MPUSEGB1 <-> 3,
  MPUSAM <-> 4,
  MPUIPC0 <-> 5,
  MPUIPSEGB2 <-> 6,
  MPUIPSEGB1 <-> 7,
}
register MPUCTL0_reg : wordBits
register MPUCTL1_reg : wordBits
register MPUSEGB2_reg : wordBits
register MPUSEGB1_reg : wordBits
register MPUSAM_reg : wordBits
register MPUIPC0_reg : wordBits
register MPUIPSEGB2_reg : wordBits
register MPUIPSEGB1_reg : wordBits
val is_mpu_reg_addr : (Address) -> bool
function is_mpu_reg_addr(addr) =
  let off : int = unsigned(addr) - unsigned(0x05A0) in
  off >= 0 & off < 16
val read_mpu_reg_byte : (Address) -> byteBits
function read_mpu_reg_byte(addr) =
  let idx : int = unsigned(addr >> 1) - unsigned(0x05A0 >> 1) in
  if idx >= 0 & idx < 8 then
  let w : wordBits = match mpu_register_index(idx) {
    MPUCTL0 =>
      ((MPUCTL0_reg & 0x00FF) | 0x9600)
      & 0b1111111100010011,
    MPUCTL1 => MPUCTL1_reg & 0b0000000000011111,
    MPUSEGB2 => MPUSEGB2_reg,
    MPUSEGB1 => MPUSEGB1_reg,
    MPUSAM => MPUSAM_reg,
    MPUIPC0 => MPUIPC0_reg & 0b0000000011100000,
    MPUIPSEGB2 => MPUIPSEGB2_reg,
    MPUIPSEGB1 => MPUIPSEGB1_reg,
  }
  in match addr[0..0] {
    0b0 => w[7..0],
    0b1 => w[15..8]
  }
  else { assert(false); 0x00 }
val is_lockable_mpu_reg : (mpu_register_name) -> bool
function is_lockable_mpu_reg(reg) =
  match reg {
    MPUCTL0 => true,
    MPUSEGB2 => true,
    MPUSEGB1 => true,
    MPUSAM => true,
    _ => false
  }
val is_ipe_reg : (mpu_register_name) -> bool
function is_ipe_reg(reg) =
  match reg {
    MPUIPC0 => true,
    MPUIPSEGB1 => true,
    MPUIPSEGB2 => true,
    _ => false
  }
val write_mpu_reg_byte : (Address, byteBits) -> unit
function write_mpu_reg_byte(addr, v) =
  let idx : int = (unsigned(addr >> 1) - unsigned(0x05A0 >> 1)) in
  let low_byte = addr[0..0] == 0b0 in
  if idx >= 0 & idx < 8 then
    let reg = mpu_register_index(idx) in
    let reg_is_not_MPUCTL0 : bool = match reg { MPUCTL0 => false, _ => true } in
    if ~(MPUCTL0_reg[15 .. 8] == 0xA5)
       & (reg_is_not_MPUCTL0 | low_byte)
    then throw power_up_clear()
    else if (is_lockable_mpu_reg(reg)
             & MPUCTL0_reg[1 .. 1] == 0b1)
          | (is_ipe_reg(reg) & MPUIPC0_reg[7 .. 7] == 0b1)
    then ()
    else if low_byte
    then match reg {
      MPUCTL0 => MPUCTL0_reg[7..0] = v,
      MPUCTL1 => MPUCTL1_reg[7..0] = v,
      MPUSEGB2 => MPUSEGB2_reg[7..0] = v,
      MPUSEGB1 => MPUSEGB1_reg[7..0] = v,
      MPUSAM => MPUSAM_reg[7..0] = v,
      MPUIPC0 => MPUIPC0_reg[7..0] = v,
      MPUIPSEGB2 => MPUIPSEGB2_reg[7..0] = v,
      MPUIPSEGB1 => MPUIPSEGB1_reg[7..0] = v,
    }
    else match reg {
      MPUCTL0 => MPUCTL0_reg[15..8] = v,
      MPUCTL1 => MPUCTL1_reg[15..8] = v,
      MPUSEGB2 => MPUSEGB2_reg[15..8] = v,
      MPUSEGB1 => MPUSEGB1_reg[15..8] = v,
      MPUSAM => MPUSAM_reg[15..8] = v,
      MPUIPC0 => MPUIPC0_reg[15..8] = v,
      MPUIPSEGB2 => MPUIPSEGB2_reg[15..8] = v,
      MPUIPSEGB1 => MPUIPSEGB1_reg[15..8] = v,
    }
  else assert(false)
val ipe_lower : (unit) -> int
function ipe_lower() = unsigned(MPUIPSEGB1_reg) * 16
val ipe_higher : (unit) -> int
function ipe_higher() = unsigned(MPUIPSEGB2_reg) * 16
val in_ipe_segment : (Address) -> bool
function in_ipe_segment(addr) =
  ipe_lower() <= unsigned(addr)
  & unsigned(addr) < ipe_higher()
val in_ivt_or_rv : (Address) -> bool
function in_ivt_or_rv(addr) =
  unsigned(0xFF80) <= unsigned(addr)
  & unsigned(addr) <= unsigned(0xFFFF)
val check_ipe_access : (Address, bool) -> bool
function check_ipe_access(addr, jump) =
  let pc = unsigned(PC_reg) in
  MPUIPC0_reg[6 .. 6] == 0b0
  |
    ~(in_ipe_segment(addr))
  |
    (ipe_lower() + 8 <= pc & pc < ipe_higher())
  |
    (jump & unsigned(addr) == ipe_lower() + 8)
val check_byte_access : (Address, bool) -> unit
function check_byte_access(addr, jump) =
  if ~(check_ipe_access(addr, jump)) then throw ipe_violation(addr)

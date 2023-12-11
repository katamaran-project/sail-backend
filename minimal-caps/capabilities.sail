let default_capability : Capability = struct {
  cap_permission = RW,
  cap_begin = sail_zeros(64),
  cap_end = 0xffffffffffffffff,
  cap_cursor = sail_zeros(64)
}

function is_sub_perm(p, p') : (Permission, Permission) -> bool = {
  match p {
    O => true,
    E => match p' {
           O => false,
	   _ => true
	 },
    R => match p' {
      O => false,
      E => false,
      _ => true
    },
    RW => match p' {
      RW => true,
      _  => false
    }
  }
}

function writeAllowed(p) : Permission -> bool =
  is_sub_perm(RW, p)

function readAllowed(p) : Permission -> bool =
  is_sub_perm(R, p)

function withinBounds(c) : Capability -> bool = {
  let cursor = unsigned(c.cap_cursor);
  let begin = unsigned(c.cap_begin);
  let top = unsigned(c.cap_end);
  (begin <= cursor) & (cursor <= top)
}

function isWithinRange(b', e', b, e) : (address, address, address, address) -> bool =
  unsigned(b) <= unsigned(b') & unsigned(e') <= unsigned(e)

val read_mem : Capability -> word
function read_mem(c) = {
  let p = readAllowed(c.cap_permission);
  assert(p, "Err: [read_mem] no read permission");
  let q = withinBounds(c);
  assert(q, "Err: [read_mem] out of bounds");
  MEMr(c.cap_cursor);
}

val write_mem : (Capability, word) -> unit
function write_mem(c, w) = {
  let p = writeAllowed(c.cap_permission);
  assert(p, "Err: [write_mem] no read permission");
  let q = withinBounds(c);
  assert(q, "Err: [write_mem] out of bounds");
  assert(MEMw(c.cap_cursor, w))
}

function permToString(p) : Permission -> string = {
  match p {
    O => "O ",
    E => "E ",
    R => "R ",
    RW => "RW"
  }
}

mapping perm_bits : Permission <-> bits(3) = {
  O  <-> 0b000,
  R  <-> 0b010,
  RW <-> 0b011,
  E  <-> 0b100,
}

function capToString(c) : Capability -> string = {
  concat_str("perm: ",
  concat_str(permToString(c.cap_permission), 
  concat_str(" begin: ",
  concat_str(bits_str(c.cap_begin),
  concat_str(" end: ",
  concat_str(bits_str(c.cap_end),
  concat_str(" cursor: ", bits_str(c.cap_cursor))))))))
}
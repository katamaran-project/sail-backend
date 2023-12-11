type address     = bits(64)
let address_size = 64

type integer     = bits(64)
let integer_size = 64

type cap_bits = bits(194)
let cap_size  = 194

enum Permission = O | R | RW | E

type instruction = bits(32)

struct Capability = {
  cap_permission: Permission,
  cap_begin:      address,
  cap_end:        address,
  cap_cursor:     address,
}

union word = {
  Cap : Capability,
  Num : integer
}

register PC : Capability
register R0 : word
register R1 : word
register R2 : word
register R3 : word

function regname(i) : int -> string = concat_str_dec("R", i)

// let registers : vector(4, dec, register(word)) = [ ref R3, ref R2, ref R1, ref R0 ]

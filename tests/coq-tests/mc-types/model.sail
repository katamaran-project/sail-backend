default Order dec

$include <prelude.sail>


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

register R0 : word = Num(sail_zeros(64))
register R1 : word = Num(sail_zeros(64))
register R2 : word = Num(sail_zeros(64))
register R3 : word = Num(sail_zeros(64))
register PC : Capability

function regname(i) : int -> string = concat_str_dec("R", i)

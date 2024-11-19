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
register PC : Capability = struct {
                             cap_permission = O,
                             cap_begin      = sail_zeros(64),
                             cap_end        = sail_zeros(64),
                             cap_cursor     = sail_zeros(64)
                           }

// function regname(i) : int -> string = concat_str_dec("R", i)

val nextPC : unit -> Capability
function nextPC() = {
  let cursor = PC.cap_cursor in
  {PC with cap_cursor = cursor + 4}
}

val updatePC : unit -> unit
function updatePC() = {
  let c = nextPC() in
  PC = c
}

val addPC : integer -> unit
function addPC(offset) = {
  let cursor = PC.cap_cursor in
  PC = {PC with cap_cursor = cursor + offset}
}

val MEMw : (address, word) -> bool
function MEMw(addr, v) = true

val MEMr : address -> word
function MEMr(addr) = Num(sail_zeros(integer_size))

val MEMri : address -> instruction
function MEMri(addr) = sail_zeros(32)

val not : bool -> bool
function not(b) = not_bool(b)

scattered union exception

union clause exception = ExpectedCapabilityRegisterContents : unit
union clause exception = ExpectedNumberRegisterContents : unit
union clause exception = CapabilityDoesNotHaveWritePermission : unit
union clause exception = CapabilityCursorCannotBeModified : unit
union clause exception = CINCOFFSETOnEnterCapability : unit
union clause exception = CSETBOUNDSOnEnterCapability : unit
union clause exception = Fail : unit

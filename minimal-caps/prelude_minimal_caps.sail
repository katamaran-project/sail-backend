$include <regfp.sail>
$include "types.sail"

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

val not = pure {coq:"negb", _:"not"} : bool -> bool

scattered union exception

union clause exception = ExpectedCapabilityRegisterContents : unit
union clause exception = ExpectedNumberRegisterContents : unit
union clause exception = CapabilityDoesNotHaveWritePermission : unit
union clause exception = CapabilityCursorCannotBeModified : unit
union clause exception = CINCOFFSETOnEnterCapability : unit
union clause exception = CSETBOUNDSOnEnterCapability : unit
union clause exception = Fail : unit
default Order dec

$include <prelude.sail>


type address     = bits(64)
let address_size = 64


struct Capability = {
  cap_cursor:     address,
}

register PC : Capability = struct { cap_cursor = sail_zeros(64) }

val nextPC : unit -> Capability
function nextPC() = {
  let cursor = PC.cap_cursor in
  {PC with cap_cursor = cursor + 4}
}

struct Capability = {
  cap_cursor: int,
}

register PC : Capability = struct { cap_cursor = 0 }

val nextPC : unit -> Capability
function nextPC() = {
  let cursor = 1 in
  {PC with cap_cursor = cursor}
}

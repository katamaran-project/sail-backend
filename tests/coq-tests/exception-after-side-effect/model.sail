default Order dec

$include <prelude.sail>

union exception = {
  Error_bad : unit
}

register status : bits(8)

/*
   As CHERIoT's wX: a register is written, then the throw skips the rest of the function.
   The write stays; step's handler sees it.
*/
val write_then_check : int -> unit
function write_then_check(x) = {
  status = 0x01;
  match x {
    0 => (),
    1 => status = 0x02,
    _ => throw(Error_bad())
  };
  status = 0x03
}

val step : int -> bits(8)
function step(x) = {
  try write_then_check(x) catch {
    Error_bad() => status = 0xFF
  };
  status
}

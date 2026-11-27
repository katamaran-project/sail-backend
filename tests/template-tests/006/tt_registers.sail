enum mode = { Off, On }

register current : mode

val toggle : mode -> mode
function toggle(m) = match m {
  Off => On,
  On  => Off
}

// Sail generates the accessors of a bitfield (Mk_flags, _get_flags_Z, ...) without a source
// location; they go to the file of the bitfield, Machine/tt_registers.v
bitfield flags : bits(2) = {
  Z : 0,
  C : 1
}

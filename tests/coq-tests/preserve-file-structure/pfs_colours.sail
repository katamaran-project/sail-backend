// A register whose type comes from another file, a loop (its helper function goes
// to this file too), and a call to neq_bool from Sail's library (which goes to machine.v)

register current : pixel

val brightness : pixel -> bits(8)
function brightness(p) = {
  match p.c {
    Red => p.v,
    Green => max_value,
    Blue => 0x00
  }
}

// the number of set bits, with a loop
val count_ones : bits(8) -> nat
function count_ones(v) = {
  result : nat = 0;
  foreach (i from 0 to 7)
    if v[i] == bitone then result = result + 1;
  result
}

val is_dark : pixel -> bool
function is_dark(p) = (p.v == 0x00) != false

// A bitfield: Sail generates its accessors (Mk_status, _get_status_ON, ...) without a source location;
// they go to the file of the bitfield, this one
bitfield status : bits(8) = {
  ON    : 0,
  LEVEL : 7 .. 1
}

register display : status

val is_on : unit -> bool
function is_on() = display[ON] == 0b1

default Order dec

$include <prelude.sail>


/* Vector literals whose elements are single bits computed at run time; in Sail, bit is bitvector(1). */

val to_bit : bool -> bit
function to_bit(b) = if b then bitone else bitzero


val single : bool -> bits(1)
function single(a) = [to_bit(a)]


val pack : (bool, bool, bool) -> bits(3)
function pack(a, b, c) = [to_bit(a), to_bit(b), to_bit(c)]


val pack_mixed : (bool, bool) -> bits(6)
function pack_mixed(a, b) = [bitone, to_bit(a), bitzero, bitzero, to_bit(b), bitone]


val all_known : unit -> bits(3)
function all_known() = [bitone, bitzero, bitone]

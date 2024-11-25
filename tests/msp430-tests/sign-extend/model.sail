default Order dec
$include <prelude.sail>
$include <string.sail>
$include <smt.sail>
$include <elf.sail>

val sign_extend_16_32 : bits(16) -> bits(32)
function sign_extend_16_32(v) = sail_sign_extend(v, 32)

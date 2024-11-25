default Order dec
$include <prelude.sail>
$include <string.sail>
$include <smt.sail>
$include <elf.sail>


val zero_extend_12_28 : bits(12) -> bits(28)
function zero_extend_12_28(v) = sail_zero_extend(v, 28)

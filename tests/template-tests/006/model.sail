default Order dec

$include <prelude.sail>

// With preserve-file-structure, the functions of each included file go to Machine/<file>.v,
// keeping its directory: those of I/tt_steps.sail go to Machine/I/tt_steps.v

$include "tt_registers.sail"
$include "I/tt_steps.sail"

val main : unit -> unit
function main() = step()

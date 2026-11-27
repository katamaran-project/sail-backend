default Order dec

$include <prelude.sail>

// A scattered union and a scattered function whose clauses are in other files.
// With preserve-file-structure, each clause of execute (split into execute_<constructor>)
// goes to the file of the clause, and execute itself to model.sail's file.

scattered union ast

val execute : ast -> bits(8)
scattered function execute

$include "pfs_increment.sail"
$include "pfs_invert.sail"

end ast
end execute

val run : unit -> bits(8)
function run() = execute(Increment(0x01)) + execute(Invert(0x03))

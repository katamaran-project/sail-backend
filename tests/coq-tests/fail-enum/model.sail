default Order dec

$include <prelude.sail>


enum GBU = { Good, Bad, Ugly }

val foo : unit -> GBU
function foo() = let _ : unit = assert(false, "oh no") in exit(())

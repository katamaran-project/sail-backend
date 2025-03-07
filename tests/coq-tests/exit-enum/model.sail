default Order dec

$include <prelude.sail>


enum City = { Paris, London }

val foo : unit -> City
function foo() = exit(())
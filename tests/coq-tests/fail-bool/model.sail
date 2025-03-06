val foo : unit -> bool
function foo() = let _ : unit = assert(false, "oh no") in exit(())

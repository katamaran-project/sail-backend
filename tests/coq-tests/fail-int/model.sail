val foo : unit -> int
function foo() = let _ : unit = assert(false, "oh no") in exit(())

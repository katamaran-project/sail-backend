default Order dec
$include <prelude.sail>

// String literals with double quotes: Rocq writes " inside a string as ""
val quoted : unit -> string
function quoted() = "an odd \" quote, a *) and a (*"

val printed : unit -> unit
function printed() = print_endline("an odd \" quote")

val asserted : int -> int
function asserted(x) = { assert(x > 0, "x is not \"positive\""); x }

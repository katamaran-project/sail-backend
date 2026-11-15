default Order dec
$include <prelude.sail>

union exception = { Oops : unit }

// Untranslated (try); its listing in base.v has an odd number of quotes and comment delimiters
val guarded : unit -> int
function guarded() =
  try { print_endline("an odd \" quote, a *) and a (*"); 1 } catch { Oops() => 0 }

val answer : unit -> int
function answer() = 42

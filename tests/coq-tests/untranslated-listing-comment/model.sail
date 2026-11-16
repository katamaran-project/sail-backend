default Order dec
$include <prelude.sail>

// Untranslated (foreach); its listing in base.v has an odd number of quotes and comment delimiters
val guarded : unit -> int
function guarded() = {
  var n : int = 0;
  foreach (i from 0 to 3) { print_endline("an odd \" quote, a *) and a (*"); n = n + i };
  n
}

val answer : unit -> int
function answer() = 42

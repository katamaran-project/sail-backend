default Order dec

$include <prelude.sail>


val string_list : unit -> list(string)
function string_list() = [|"This", "is", "fine."|]

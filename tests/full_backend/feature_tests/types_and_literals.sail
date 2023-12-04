val unit_lit : unit -> unit
function unit_lit() = ()

val bool_pair : unit -> (bool, bool)
function bool_pair() = (true, false)

val int_tupple : unit -> (int, int, int)
function int_tupple() = (-1, 0, 1)

val string_list : unit -> list(string)
function string_list() = [|"This", "is", "fine."|]

val mix_tuple : unit -> (int, unit, (bool, int), string)
function mix_tuple() = (100, (), (false, -2), "true")
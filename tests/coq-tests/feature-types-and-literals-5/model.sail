default Order dec

$include <prelude.sail>


val mix_tuple : unit -> (int, unit, (bool, int), string)
function mix_tuple() = (100, (), (false, -2), "true")

default Order dec

$include <prelude.sail>


val int_int : unit -> (int, int)
function int_int() = {
  (1, 2)
}

val int_bool : unit -> (int, bool)
function int_bool() = {
  (1, false)
}

val bool_int : unit -> (bool, int)
function bool_int() = {
  (true, 5)
}

val string_string : unit -> (string, string)
function string_string() = {
  ("a", "b")
}

val string_unit : unit -> (string, unit)
function string_unit() = {
  ("a", ())
}

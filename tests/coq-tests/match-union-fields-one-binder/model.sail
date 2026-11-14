default Order dec

$include <prelude.sail>

/*
  A pattern C(e) that binds all fields of a constructor with several fields
  to one variable: e is bound to the tuple of the fields.
  The fields are bound to fresh variables and the tuple is rebuilt.
*/
enum Kind = { KData, KCap }

union Result = {
  Ok    : int,
  Pair  : (int, bool),
  Three : (int, bool, Kind),
}

val first_of_pair : ((int, bool)) -> int
function first_of_pair(p) = {
  let (x, _) = p in x
}

val use_pair : Result -> int
function use_pair(r) = match r {
  Ok(n)    => n,
  Pair(p)  => first_of_pair(p),
  Three(_) => 0,
}

val get_three : Result -> (int, bool, Kind)
function get_three(r) = match r {
  Three(t) => t,
  _        => (0, false, KData),
}

/* As in CHERIoT: a union with a type parameter, used at a tuple type. */
type control_error = (Kind, bits(5))

union Check('a : Type) = {
  Check_OK    : bits(32),
  Check_Error : 'a,
}

val handle_error : control_error -> unit
function handle_error(err) = ()

val check : Check(control_error) -> bool
function check(c) = match c {
  Check_OK(_)    => true,
  Check_Error(e) => { handle_error(e); false },
}

default Order dec

$include <prelude.sail>


val id_int : int -> int
function id_int(x) = x

val id_bool : bool -> bool
function id_bool(x) = x

val id_unit : unit -> unit
function id_unit(x) = x

overload id = { id_int, id_bool, id_unit }


val foo_int : int -> int
function foo_int(x) = id(x)


val foo_bool : bool -> bool
function foo_bool(x) = id(x)


val foo_unit : unit -> unit
function foo_unit(x) = id(x)

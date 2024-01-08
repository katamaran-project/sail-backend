val int_args :  ((int), int) -> int
function int_args(n, (m)) = 1

val string_args : string -> unit
function string_args(s) = ()

val bool_args : (bool) -> unit
function bool_args(b) = ()

val unit_args : unit -> unit
function unit_args() = ()

val list_args : list(unit) -> unit
function list_args(l) = ()
val info     : string -> unit
val debug    : string -> unit
val surround : (string -> unit) -> string -> (unit -> 'a) -> 'a

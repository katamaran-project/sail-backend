val info     : string -> unit
val debug    : string -> unit
val surround : (string -> unit) -> Lexing.position -> string -> (unit -> 'a) -> 'a

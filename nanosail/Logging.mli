val info     : Lexing.position -> string lazy_t -> unit
val debug    : Lexing.position -> string lazy_t -> unit
val warning  : Lexing.position -> string lazy_t -> unit
val surround : (string lazy_t -> unit) -> Lexing.position -> string -> (unit -> 'a) -> 'a

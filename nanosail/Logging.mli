val info     : string lazy_t -> unit
val debug    : string lazy_t -> unit
val warning  : string lazy_t -> unit
val surround : (string lazy_t -> unit) -> Lexing.position -> string -> (unit -> 'a) -> 'a

val info     : Lexing.position -> string lazy_t -> unit
val debug    : Lexing.position -> string lazy_t -> unit
val warning  : Lexing.position -> string lazy_t -> unit
val error    : Lexing.position -> string lazy_t -> unit
val surround : Lexing.position -> (Lexing.position -> string lazy_t -> unit) -> string lazy_t -> (unit -> 'a) -> 'a

val increase_indentation       : unit -> unit
val decrease_indentation       : unit -> unit
val with_increased_indentation : (unit -> 'a) -> 'a

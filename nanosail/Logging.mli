module VerbosityLevel : sig
  type t

  val to_string   : t -> string
  val from_int    : int -> t
  val should_show : filter_level:t -> message_level:t -> bool

  val quiet       : t
  val error       : t
  val warning     : t
  val info        : t
  val debug       : t
  val default     : t
end

val verbosity_level : VerbosityLevel.t ConfigLib.Setting.t

val info     : Lexing.position -> string lazy_t -> unit
val debug    : Lexing.position -> string lazy_t -> unit
val warning  : Lexing.position -> string lazy_t -> unit
val error    : Lexing.position -> string lazy_t -> unit
val surround : Lexing.position -> (Lexing.position -> string lazy_t -> unit) -> string lazy_t -> (unit -> 'a) -> 'a

val increase_indentation        : unit -> unit
val decrease_indentation        : unit -> unit
val with_increased_indentation  : (unit -> 'a) -> 'a
val create_indentation_restorer : unit -> unit -> unit

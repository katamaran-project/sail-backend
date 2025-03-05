type t

val to_string   : t -> string
val of_int      : int -> t
val should_show : filter_level:t -> message_level:t -> bool

val quiet       : t
val error       : t
val warning     : t
val info        : t
val debug       : t
val default     : t

val to_message  : t -> PP.document

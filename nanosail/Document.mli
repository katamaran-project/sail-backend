type t

val space        : t
val empty        : t

val to_string    : t -> string
val separate     : t -> t list -> t list
val hanging_list : t list -> t

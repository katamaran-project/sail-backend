type t

val equal             : t -> t -> bool
val pp                : t -> PP.t
val pp_diff           : (PP.t -> PP.t) -> compared_with : t -> printed : t -> PP.t
val to_string         : t -> string

val mk_int            : int -> t
val mk_bool           : bool -> t
val mk_string         : string -> t
val mk_application    : ?positional:t list -> ?keyword:(string * t) list -> string -> t
val mk_symbol         : string -> t
val mk_list           : t list -> t
val mk_nil            : t
val mk_option         : t option -> t
val mk_ocaml_location : Lexing.position -> t
val mk_sail_location  : Libsail.Parse_ast.l -> t

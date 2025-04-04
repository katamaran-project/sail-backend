module Context : sig
  type t
end

type 'a t

module Error : sig
  type t =
    | NotYetImplemented of Lexing.position * Libsail.Ast.l * string option
    | Failure           of Lexing.position * string

  val to_string : t -> string
end

type 'a result =
  | Success of 'a
  | Failure of Error.t


val bind                           : 'a t -> ('a -> 'b t) -> 'b t
val act                            : (unit -> 'a) -> 'a t
val return                         : 'a -> 'a t
val error                          : Error.t -> 'a t
val not_yet_implemented            : ?message : string -> Lexing.position -> Libsail.Ast.l -> 'a t
val check                          : Lexing.position -> bool -> string Lazy.t -> unit t
val fail                           : Lexing.position -> string -> 'a t
val recover                        : 'a t -> (Error.t -> 'a t) -> 'a t
val run                            : 'a t -> ('a * Ast.Type.t list list Ast.Identifier.Map.t) result


val store_definition               : Ast.Definition.t -> unit t
val select_definitions             : (Ast.Definition.t, 'a) Ast.Definition.Select.selector -> 'a list t
val lookup_definition              : (Ast.Definition.t, 'a) Ast.Definition.Select.selector -> 'a t
val lookup_definition_opt          : (Ast.Definition.t, 'a) Ast.Definition.Select.selector -> 'a option t
val lookup_variant_by_constructor  : Ast.Identifier.t -> Ast.Definition.Type.Variant.t option t
val lookup_register_type           : Ast.Identifier.t -> Ast.Type.t option t
val is_register                    : Ast.Identifier.t -> bool t


val generate_unique_int            : int t
val generate_unique_identifier     : ?prefix:string -> ?suffix:string -> ?underscore:bool -> unit -> Ast.Identifier.t t
val generate_unique_identifiers    : ?prefix:string -> ?suffix:string -> ?underscore:bool -> int -> Ast.Identifier.t list t


val map                            : f:('a -> 'b t) -> 'a list -> 'b list t
val filter_map                     : f:('a -> 'b option t) -> 'a list -> 'b list t
val fold_left                      : f:('a -> 'b -> 'a t) -> init:'a -> 'b list -> 'a t
val iter                           : f:('a -> unit t) -> 'a list -> unit t
val lift                           : f:('a -> 'b) -> 'a t -> 'b t
val lift_option                    : 'a t option -> 'a option t
val repeat                         : int -> f:('a t) -> 'a list t

val log                            : Lexing.position -> (Lexing.position -> PP.t lazy_t -> unit) -> PP.t lazy_t -> unit t
val translation_block              : Lexing.position -> PP.t -> 'a t -> 'a t


val register_polymorphic_function_call_type_arguments : Ast.Identifier.t -> Ast.Type.t list -> unit t

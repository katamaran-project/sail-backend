module Context : sig
  type t
end

type 'a t

module Error : sig
  type t =
    | NotYetImplemented of Lexing.position * Libsail.Ast.l * string option
    | AssertionFailure of Lexing.position * string
end

type 'a result =
  | Success of 'a
  | Failure of Error.t


val bind                       : 'a t -> ('a -> 'b t) -> 'b t
val return                     : 'a -> 'a t
val error                      : Error.t -> 'a t
val not_yet_implemented        : ?message : string -> Lexing.position -> Libsail.Ast.l -> 'a t
val check                      : Lexing.position -> bool -> string Lazy.t -> unit t
val fail                       : Lexing.position -> string -> 'a t
val recover                    : 'a t -> (Error.t -> 'a t) -> 'a t
val run                        : 'a t -> 'a result * Context.t
val debug_error                : 'a t -> 'a t


val register                   : Ast.definition -> unit t
val lookup_type                : (Ast.type_definition -> 'a option) -> Ast.Identifier.t -> 'a option t
val lookup_register_type       : Ast.Identifier.t -> Ast.nanotype option t
val generate_unique_int        : int t
val generate_unique_identifier : string -> Ast.Identifier.t t
val is_register                : Ast.Identifier.t -> bool t


val map                        : f:('a -> 'b t) -> 'a list -> 'b list t
val fold_left                  : f:('a -> 'b -> 'a t) -> init:'a -> 'b list -> 'a t
val iter                       : f:('a -> unit t) -> 'a list -> unit t
val lift                       : f:('a -> 'b) -> 'a t -> 'b t

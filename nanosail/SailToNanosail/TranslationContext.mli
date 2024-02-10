module Context : sig
  type t
end

type 'a t

type error =
  | NotYetImplemented of Lexing.position * Libsail.Ast.l * string option
  | AssertionFailure of Lexing.position * string

type 'a result =
  | Success of 'a | Failure of error

val bind                       : 'a t -> ('a -> 'b t) -> 'b t
val return                     : 'a -> 'a t
val not_yet_implemented        : ?message : string -> Lexing.position -> Libsail.Ast.l -> 'a t
val check                      : Lexing.position -> bool -> string Lazy.t -> unit t
val fail                       : Lexing.position -> string -> 'a t
val recover                    : 'a t -> (error -> 'a t) -> 'a t
val run                        : 'a t -> 'a result * Context.t

val register                   : Ast.definition -> unit t
val register_type              : Ast.type_definition -> unit t   (* todo remove this, only export register *)
val lookup_type                : (Ast.type_definition -> 'a option) -> string -> 'a option t
val generate_unique_identifier : string -> string t

val map                        : f:('a -> 'b t) -> 'a list -> 'b list t
val fold_left                  : f:('a -> 'b -> 'a t) -> init:'a -> 'b list -> 'a t
val iter                       : f:('a -> unit t) -> 'a list -> unit t
val lift                       : f:('a -> 'b) -> 'a t -> 'b t

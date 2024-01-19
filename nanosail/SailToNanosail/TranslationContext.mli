(* TODO cleanup *)

module Context : sig
  type t
end

type 'a t

type error = NotYetImplemented of Lexing.position * Libsail.Ast.l * string option

type 'a result = Success of 'a | Failure of error


val return : 'a -> 'a t
val not_yet_implemented : ?message : string -> Lexing.position -> Libsail.Ast.l -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val recover : 'a t -> (error -> 'a t) -> 'a t
val run : 'a t -> 'a result * Context.t

val register_type : Ast.type_definition -> unit t
val map : ('a -> 'b t) -> 'a list -> 'b list t
val fold_left : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

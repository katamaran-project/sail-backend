type 'a t

module Context : sig
  type t
end

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val run : 'a t -> 'a * Context.t

val register_type : Ast.type_definition -> unit t
val map : ('a -> 'b t) -> 'a list -> 'b list t
val fold_left : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

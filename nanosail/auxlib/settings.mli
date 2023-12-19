type 'a t

val create : 'a -> 'a t
val get    : 'a t -> 'a
val set    : 'a t -> 'a -> unit

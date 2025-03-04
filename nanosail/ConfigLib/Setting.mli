type 'a t

val mk     : 'a -> 'a t
val get    : 'a t -> 'a
val set    : 'a t -> 'a -> unit
val update : 'a t -> f:('a -> 'a) -> unit

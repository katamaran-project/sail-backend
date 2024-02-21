type annotation = PPrint.document (* todo add wrapper *)

type 'a t

val return              : 'a -> 'a t
val bind                : 'a t -> ('a -> 'b t) -> 'b t

val create_annotation   : PPrint.document -> int t
val not_yet_implemented : ?message:string -> Lexing.position -> annotation t
val collect_annotations : 'a t -> ('a * annotation list)

val map                 : f:('a -> 'b t) -> 'a list -> 'b list t
val iter                : f:('a -> unit t) -> 'a list -> unit t
val lift                : f:('a -> 'b) -> 'a t -> 'b t

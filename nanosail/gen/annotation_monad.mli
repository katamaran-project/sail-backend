type annotation = PPrint.document

type 'a t

val return              : 'a -> 'a t
val bind                : 'a t -> ('a -> 'b t) -> 'b t

val create_annotation   : PPrint.document -> int t
val not_yet_implemented : Lexing.position -> annotation t
val collect_annotations : 'a t -> ('a * annotation list)
                                  
val map                 : ('a -> 'b t) -> 'a list -> 'b list t
val iter                : ('a -> unit t) -> 'a list -> unit t

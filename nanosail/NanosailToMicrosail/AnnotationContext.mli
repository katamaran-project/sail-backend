type annotation = Annotation of PPrint.document

type 'a t

val return                          : 'a -> 'a t
val bind                            : 'a t -> ('a -> 'b t) -> 'b t
val compose                         : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t

val create_annotation_from_document : PPrint.document -> int t
val create_annotation_from_string   : string -> int t
val document_of_annotation          : annotation -> PP.document
                                             
val not_yet_implemented             : ?message:string -> Lexing.position -> PP.document t
val collect_annotations             : 'a t -> ('a * annotation list)
val drop_annotations                : 'a t -> 'a

val map                             : f:('a -> 'b t) -> 'a list -> 'b list t
val iter                            : f:('a -> unit t) -> 'a list -> unit t
val lift                            : f:('a -> 'b) -> 'a t -> 'b t

type 'a t

val act    : (unit -> 'a) -> 'a t
val return : 'a -> 'a t
val bind   : 'a t -> ('a -> 'b t) -> 'b t
val fail   : string -> 'a t

val generate            : PP.document t -> PP.document
val block               : PP.document t -> PP.document t
val add_annotation      : PP.document -> int t
val add_comment         : PP.document -> unit t
val not_yet_implemented : ?message : string -> Lexing.position -> PP.document t
val generation_block    : Lexing.position -> PP.document -> PP.document   -> PP.document t
val generation_block'   : Lexing.position -> PP.document -> PP.document t -> PP.document t


val pp_inductive_type :
  PP.document ->
  ?parameters : (PP.document * PP.document) list ->
  PP.document ->
  ((?parameters:PPrint.document -> ?typ:PPrint.document -> PPrint.document -> unit t) -> 'a t) ->
  PP.document t

val add_original_definition  : Libsail.Type_check.tannot Libsail.Ast.def -> unit t
val add_original_definitions : Libsail.Type_check.tannot Libsail.Ast.def list -> unit t

val pp_sail_definition : Libsail.Type_check.tannot Libsail.Ast.def -> PP.document

val log : (string -> unit) -> string -> unit t

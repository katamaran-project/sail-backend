type 'a t

val act     : (unit -> 'a) -> 'a t
val return  : 'a -> 'a t
val bind    : 'a t -> ('a -> 'b t) -> 'b t
val fail    : Lexing.position -> string -> 'a t
val recover : 'a t -> (string -> 'a t) -> 'a t

val generate            : Ast.program -> PP.document t -> PP.document
val block               : PP.document t -> PP.document t
val add_annotation      : PP.document -> int t
val add_comment         : PP.document -> unit t
val not_yet_implemented : ?message : string -> Lexing.position -> PP.document t
val generation_block    : Lexing.position -> string -> PP.document t -> PP.document t
val pp_annotate         : Lexing.position -> PP.document t -> PP.document t
val pp_annotate'        : Lexing.position -> string -> PP.document t -> PP.document t

(* todo move this elsewhere *)
val pp_inductive_type :
  PP.document ->
  ?parameters : (PP.document * PP.document) list ->
  PP.document ->
  ((?parameters:PP.document -> ?typ:PP.document -> PP.document -> unit t) -> 'a t) ->
  PP.document t

val add_original_definition  : Sail.sail_definition -> unit t
val add_original_definitions : Sail.sail_definition list -> unit t

val log : Lexing.position -> (Lexing.position -> string lazy_t -> unit) -> string lazy_t -> unit t

val get_program : Ast.program t

val lookup_type_abbreviation_definition : Ast.Identifier.t -> Ast.Definition.Type.Abbreviation.t option t

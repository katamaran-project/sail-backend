type 'a t

val act     : (unit -> 'a) -> 'a t
val return  : 'a -> 'a t
val bind    : 'a t -> ('a -> 'b t) -> 'b t
val fail    : Lexing.position -> string -> 'a t
val recover : 'a t -> (string -> 'a t) -> 'a t
val map     : f:('a -> 'b t) -> 'a list -> 'b list t

val generate            : Ast.Program.t -> PP.t t -> PP.t
val block               : PP.t t -> PP.t t
val add_annotation      : PP.t -> int t
val add_comment         : PP.t -> unit t
val not_yet_implemented : ?message : string -> Lexing.position -> PP.t t
val generation_block    : Lexing.position -> string -> PP.t t -> PP.t t
val pp_annotate         : Lexing.position -> PP.t t -> PP.t t
val pp_annotate'        : Lexing.position -> string -> PP.t t -> PP.t t

(* todo move this elsewhere *)
val pp_inductive_type :
  PP.t ->
  ?parameters : (PP.t * PP.t) list ->
  PP.t ->
  ((?parameters:PP.t -> ?typ:PP.t -> PP.t -> unit t) -> 'a t) ->
  PP.t t

val add_original_definition  : Sail.sail_definition -> unit t
val add_original_definitions : Sail.sail_definition list -> unit t

val log : Lexing.position -> (Lexing.position -> PP.t lazy_t -> unit) -> PP.t lazy_t -> unit t

val get_program : Ast.Program.t t

val select_definitions    : (Sail.sail_definition * Ast.Definition.t, 'a) Ast.Definition.Select.selector -> 'a list t
val lookup_definition_opt : (Sail.sail_definition * Ast.Definition.t, 'a) Ast.Definition.Select.selector -> 'a option t

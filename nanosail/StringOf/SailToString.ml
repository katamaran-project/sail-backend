(*
   Convenience module. Groups all sail_value_to_string functionality in one place.
*)

open! ExtBase


module Sail = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
  include Libsail.Type_check
end


let id              = Sail.string_of_id
let kid             = Sail.string_of_kid
let kind_aux        = Sail.string_of_kind_aux
let kind            = Sail.string_of_kind
let nexp            = Sail.string_of_nexp
let typ             = Sail.string_of_typ
let typ_arg         = Sail.string_of_typ_arg
let typ_pat         = Sail.string_of_typ_pat
let n_constraint    = Sail.string_of_n_constraint
let kinded_id       = Sail.string_of_kinded_id
let quant_item      = Sail.string_of_quant_item
let typquant        = Sail.string_of_typquant
let typschm         = Sail.string_of_typschm
let lit             = Sail.string_of_lit
let exp             = Sail.string_of_exp
let pexp            = Sail.string_of_pexp
let lexp            = Sail.string_of_lexp
let mpat            = Sail.string_of_mpat
let letbind         = Sail.string_of_letbind
let index_range     = Sail.string_of_index_range
let type_annotation = Sail.string_of_tannot


let aval (aval : 'a Sail.aval) =
  match aval with
  | AV_lit (x, _)      -> Printf.sprintf "AV_lit(%s)" (lit x)
  | AV_id (x, _)       -> Printf.sprintf "AV_id(%s,_)" (id x)
  | AV_ref (_, _)      -> Printf.sprintf "AV_ref(_,_)"
  | AV_tuple _         -> Printf.sprintf "AV_tuple(_)"
  | AV_list (_, _)     -> Printf.sprintf "AV_list(_, _)"
  | AV_vector (_, _)   -> Printf.sprintf "AV_vector(_, _)"
  | AV_record (_, _)   -> Printf.sprintf "AV_record(_, _)"
  | AV_cval (_, _)     -> Printf.sprintf "AV_cval(_, _)"


let list ~(f:'a -> string) (xs : 'a list) =
  "[" ^ String.concat ~sep:"; " (List.map ~f xs) ^ "]"


let alexp (location_expression : 'a Sail.alexp) =
  match location_expression with
  | AL_id (_, _)    -> Printf.sprintf "AL_id (_, _)"
  | AL_addr (_, _)  -> Printf.sprintf "AL_addr (_, _)"
  | AL_field (_, _) -> Printf.sprintf "AL_field (_, _)"


let rec apat (pattern : 'a Sail.apat) =
  let AP_aux (pattern, _env, _location) = pattern
  in
  match pattern with
  | AP_app (identifier, subpattern, _) -> begin
      Printf.sprintf "AP_app (%s, %s, _)" (id identifier) (apat subpattern)
    end
  | AP_tuple _       -> Printf.sprintf "AP_tuple _"
  | AP_id (_, _)     -> Printf.sprintf "AP_id (_, _)"
  | AP_global (_, _) -> Printf.sprintf "AP_global (_, _)"
  | AP_cons (_, _)   -> Printf.sprintf "AP_cons (_, _)"
  | AP_as (_, _, _)  -> Printf.sprintf "AP_as (_, _, _)"
  | AP_struct (_, _) -> Printf.sprintf "AP_struct (_, _)"
  | AP_nil _         -> Printf.sprintf "AP_nil _"
  | AP_wild _        -> Printf.sprintf "AP_wild _"


let rec aexp (expression : 'a Sail.aexp) =
  let AE_aux (expression, _annotation) = expression
  in
  match expression with
  | AE_val value                   -> Printf.sprintf "AE_val(%s)" (aval value)
  | AE_app (identifier, values, _) -> Printf.sprintf "AE_app(%s, %s, ?)" (id identifier) (list ~f:aval values)
  | AE_typ (expression, _)         -> Printf.sprintf "AE_typ(%s, ?)" (aexp expression)
  | AE_assign (_, _)               -> Printf.sprintf "AE_assign(%s, %s)" "?" "?"
  | AE_let (_, _, _, _, _, _)      -> Printf.sprintf "AE_let (_, _, _, _, _, _)"
  | AE_block (_, _, _)             -> Printf.sprintf "AE_block (_, _, _)"
  | AE_return (_, _)               -> Printf.sprintf "AE_return (_, _)"
  | AE_exit (_, _)                 -> Printf.sprintf "AE_exit (_, _)"
  | AE_throw (_, _)                -> Printf.sprintf "AE_throw (_, _)"
  | AE_if (_, _, _, _)             -> Printf.sprintf "AE_if (_, _, _, _)"
  | AE_field (_, _, _)             -> Printf.sprintf "AE_field (_, _, _)"
  | AE_match (_, _, _)             -> Printf.sprintf "AE_match (_, _, _)"
  | AE_try (_, _, _)               -> Printf.sprintf "AE_try (_, _, _)"
  | AE_struct_update (_, _, _)     -> Printf.sprintf "AE_struct_update (_, _, _)"
  | AE_for (_, _, _, _, _, _)      -> Printf.sprintf "AE_for (_, _, _, _, _, _)"
  | AE_loop (_, _, _)              -> Printf.sprintf "AE_loop (_, _, _)"
  | AE_short_circuit (_, _, _)     -> Printf.sprintf "AE_short_circuit (_, _, _)"


let pat (pattern : Sail.tannot Libsail.Ast.pat) : string =
  let Libsail.Ast.P_aux (_raw_pattern, annotation) = pattern
  in
  let typ = Libsail.Type_check.typ_of_annot annotation
  in
  Printf.sprintf "(%s : %s)" (Libsail.Ast_util.string_of_pat pattern) (Libsail.Ast_util.string_of_typ typ)


let location (location : Libsail.Parse_ast.l) =
  let string_of_ocaml_position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
    Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum
  in
  let rec string_of_location (location : Libsail.Parse_ast.l) =
    match location with
    | Unknown                 -> "UnknownLocation"
    | Unique (k, loc)         -> Printf.sprintf "UniqueLocation(%d, %s)" k (string_of_location loc)
    | Generated loc           -> Printf.sprintf "GeneratedLocation(%s)" (string_of_location loc)
    | Hint (hint, loc1, loc2) -> Printf.sprintf "HintLocation(%s, %s, %s)" hint (string_of_location loc1) (string_of_location loc2)
    | Range (pos1, pos2)      -> Printf.sprintf "Range(%s-%s)" (string_of_ocaml_position pos1) (string_of_ocaml_position pos2)
  in
  string_of_location location


let definition
    ?(buffer_initial_size : int            = 1000                )
    ?(line_width          : int            = 160                 )
    ?(ribbon_width        : float          = 1.0                 )
    (sail_definition      : Sail.typed_def                       ) : string
  =
  let buffer = Buffer.create buffer_initial_size
  in
  let document = Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)
  in
  PPrint.ToBuffer.pretty ribbon_width line_width buffer document;
  Buffer.contents buffer

open Base


let id           = Libsail.Ast_util.string_of_id
let kid          = Libsail.Ast_util.string_of_kid
let kind_aux     = Libsail.Ast_util.string_of_kind_aux
let kind         = Libsail.Ast_util.string_of_kind
let nexp         = Libsail.Ast_util.string_of_nexp
let typ          = Libsail.Ast_util.string_of_typ
let typ_arg      = Libsail.Ast_util.string_of_typ_arg
let typ_pat      = Libsail.Ast_util.string_of_typ_pat
let n_constraint = Libsail.Ast_util.string_of_n_constraint
let kinded_id    = Libsail.Ast_util.string_of_kinded_id
let quant_item   = Libsail.Ast_util.string_of_quant_item
let typquant     = Libsail.Ast_util.string_of_typquant
let typschm      = Libsail.Ast_util.string_of_typschm
let lit          = Libsail.Ast_util.string_of_lit
let exp          = Libsail.Ast_util.string_of_exp
let pexp         = Libsail.Ast_util.string_of_pexp
let lexp         = Libsail.Ast_util.string_of_lexp
let mpat         = Libsail.Ast_util.string_of_mpat
let letbind      = Libsail.Ast_util.string_of_letbind
let index_range  = Libsail.Ast_util.string_of_index_range


let aval (aval : 'a Libsail.Anf.aval) =
  match aval with
  | Libsail.Anf.AV_lit (x, _)      -> Printf.sprintf "AV_lit(%s)" (lit x)
  | Libsail.Anf.AV_id (x, _)       -> Printf.sprintf "AV_id(%s,_)" (id x)
  | Libsail.Anf.AV_ref (_, _)      -> Printf.sprintf "AV_ref(_,_)"
  | Libsail.Anf.AV_tuple _         -> Printf.sprintf "AV_tuple(_)"
  | Libsail.Anf.AV_list (_, _)     -> Printf.sprintf "AV_list(_, _)"
  | Libsail.Anf.AV_vector (_, _)   -> Printf.sprintf "AV_vector(_, _)"
  | Libsail.Anf.AV_record (_, _)   -> Printf.sprintf "AV_record(_, _)"
  | Libsail.Anf.AV_cval (_, _)     -> Printf.sprintf "AV_cval(_, _)"


let list ~(f:'a -> string) (xs : 'a list) =
  "[" ^ String.concat ~sep:"; " (List.map ~f xs) ^ "]"


let alexp (location_expression : 'a Libsail.Anf.alexp) =
  match location_expression with
  | Libsail.Anf.AL_id (_, _)    -> Printf.sprintf "AL_id (_, _)"
  | Libsail.Anf.AL_addr (_, _)  -> Printf.sprintf "AL_addr (_, _)"
  | Libsail.Anf.AL_field (_, _) -> Printf.sprintf "AL_field (_, _)"


let rec apat (pattern : 'a Libsail.Anf.apat) =
  let AP_aux (pattern, _env, _location) = pattern
  in
  match pattern with
  | Libsail.Anf.AP_app (identifier, subpattern, _) -> begin
      Printf.sprintf "AP_app (%s, %s, _)" (id identifier) (apat subpattern)
    end
  | Libsail.Anf.AP_tuple _       -> Printf.sprintf "AP_tuple _"
  | Libsail.Anf.AP_id (_, _)     -> Printf.sprintf "AP_id (_, _)"
  | Libsail.Anf.AP_global (_, _) -> Printf.sprintf "AP_global (_, _)"
  | Libsail.Anf.AP_cons (_, _)   -> Printf.sprintf "AP_cons (_, _)"
  | Libsail.Anf.AP_as (_, _, _)  -> Printf.sprintf "AP_as (_, _, _)"
  | Libsail.Anf.AP_struct (_, _) -> Printf.sprintf "AP_struct (_, _)"
  | Libsail.Anf.AP_nil _         -> Printf.sprintf "AP_nil _"
  | Libsail.Anf.AP_wild _        -> Printf.sprintf "AP_wild _"


let rec aexp (expression : 'a Libsail.Anf.aexp) =
  let AE_aux (expression, _annotation) = expression
  in
  match expression with
  | Libsail.Anf.AE_val value                   -> Printf.sprintf "AE_val(%s)" (aval value)
  | Libsail.Anf.AE_app (identifier, values, _) -> Printf.sprintf "AE_app(%s, %s, ?)" (id identifier) (list ~f:aval values)
  | Libsail.Anf.AE_typ (expression, _)         -> Printf.sprintf "AE_typ(%s, ?)" (aexp expression)
  | Libsail.Anf.AE_assign (_, _)               -> Printf.sprintf "AE_assign(%s, %s)" "?" "?"
  | Libsail.Anf.AE_let (_, _, _, _, _, _)      -> Printf.sprintf "AE_let (_, _, _, _, _, _)"
  | Libsail.Anf.AE_block (_, _, _)             -> Printf.sprintf "AE_block (_, _, _)"
  | Libsail.Anf.AE_return (_, _)               -> Printf.sprintf "AE_return (_, _)"
  | Libsail.Anf.AE_exit (_, _)                 -> Printf.sprintf "AE_exit (_, _)"
  | Libsail.Anf.AE_throw (_, _)                -> Printf.sprintf "AE_throw (_, _)"
  | Libsail.Anf.AE_if (_, _, _, _)             -> Printf.sprintf "AE_if (_, _, _, _)"
  | Libsail.Anf.AE_field (_, _, _)             -> Printf.sprintf "AE_field (_, _, _)"
  | Libsail.Anf.AE_match (_, _, _)             -> Printf.sprintf "AE_match (_, _, _)"
  | Libsail.Anf.AE_try (_, _, _)               -> Printf.sprintf "AE_try (_, _, _)"
  | Libsail.Anf.AE_struct_update (_, _, _)     -> Printf.sprintf "AE_struct_update (_, _, _)"
  | Libsail.Anf.AE_for (_, _, _, _, _, _)      -> Printf.sprintf "AE_for (_, _, _, _, _, _)"
  | Libsail.Anf.AE_loop (_, _, _)              -> Printf.sprintf "AE_loop (_, _, _)"
  | Libsail.Anf.AE_short_circuit (_, _, _)     -> Printf.sprintf "AE_short_circuit (_, _, _)"


let pat (pattern : Libsail.Type_check.tannot Libsail.Ast.pat) : string =
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
      ?(buffer_initial_size = 1000)
      ?(line_width = 160)
      ?(ribbon_width = 1.0)
      sail_definition =
  let buffer = Buffer.create buffer_initial_size
  in
  let document = Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)
  in
  PPrint.ToBuffer.pretty ribbon_width line_width buffer document;
  Buffer.contents buffer

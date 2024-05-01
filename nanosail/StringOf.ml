open Base
open Libsail.Ast_util


let id           = string_of_id
let kid          = string_of_kid
let kind_aux     = string_of_kind_aux
let kind         = string_of_kind
let nexp         = string_of_nexp
let typ          = string_of_typ
let typ_arg      = string_of_typ_arg
let typ_pat      = string_of_typ_pat
let n_constraint = string_of_n_constraint
let kinded_id    = string_of_kinded_id
let quant_item   = string_of_quant_item
let typquant     = string_of_typquant
let typschm      = string_of_typschm
let lit          = string_of_lit
let exp          = string_of_exp
let pexp         = string_of_pexp
let lexp         = string_of_lexp
let pat          = string_of_pat
let mpat         = string_of_mpat
let letbind      = string_of_letbind
let index_range  = string_of_index_range


let aval (aval : 'a Libsail.Anf.aval) =
  match aval with
  | Libsail.Anf.AV_lit (x, _)    -> Printf.sprintf "AV_lit(%s)" (lit x)
  | Libsail.Anf.AV_id (x, _)     -> Printf.sprintf "AV_id(%s,_)" (id x)
  | Libsail.Anf.AV_ref (_, _)    -> Printf.sprintf "AV_ref(_,_)"
  | Libsail.Anf.AV_tuple _       -> Printf.sprintf "AV_tuple(_)"
  | Libsail.Anf.AV_list (_, _)   -> Printf.sprintf "AV_list(_, _)"
  | Libsail.Anf.AV_vector (_, _) -> Printf.sprintf "AV_vector(_, _)"
  | Libsail.Anf.AV_record (_, _) -> Printf.sprintf "AV_record(_, _)"
  | Libsail.Anf.AV_cval (_, _)   -> Printf.sprintf "AV_cval(_, _)"


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
  let AE_aux (expression, _env, _location) = expression
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

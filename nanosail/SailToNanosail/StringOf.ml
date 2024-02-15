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

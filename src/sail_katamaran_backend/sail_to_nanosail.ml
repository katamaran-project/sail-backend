open Option
open Libsail.Ast
open Libsail.Ast_defs
open Libsail.Anf
open Libsail
open Nanosail.Ast

module Big_int = Nat_big_num

(******************************************************************************)
(* Functions from Libsail.Ast_util *)

let string_of_id (Id_aux (aux, _)) =
  match aux with
  | Id x -> x
  | _ -> " NOT YET SUPPORTED "

let string_of_lit (L_aux (lit, _)) =
  match lit with
  | L_unit -> "()"
  | L_true -> "true"
  | L_false -> "false"
  | L_num n -> Big_int.to_string n
  | L_string str -> "\"" ^ str ^ "\""
  | _ -> " NOT YET SUPPORTED "
  (*
  | L_zero -> "bitzero"
  | L_one -> "bitone"
  | L_hex n -> "0x" ^ n
  | L_bin n -> "0b" ^ n
  | L_undef -> "undefined"
  | L_real r -> r
  | L_string str -> "\"" ^ str ^ "\""
  *)

(******************************************************************************)

let ty_id_of_typ_id (Id_aux (aux, _)) =
  match aux with
  | Id "bool" -> Bool
  | Id "int"  -> Int
  | Id "list" -> List
  | Id "prod" -> Prod
  | Id "unit" -> Unit
  | _ -> Id_nys

let rec ty_of_typ (Typ_aux (typ, _)) =
  let ty_of_arg (A_aux (aux, _)) = 
    match aux with
    | A_typ typ -> ty_of_typ typ
    | _ -> Ty_nys
  in match typ with
  | Typ_id id          -> Ty_id (ty_id_of_typ_id id)
  | Typ_app (id, args) -> Ty_app (ty_id_of_typ_id id, List.map ty_of_arg args)
  | Typ_tup typs     -> Ty_app (Prod, List.map ty_of_typ typs)
  | _ -> Ty_nys

(******************************************************************************)

let ty_of_pexp (Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (_, exp) -> ty_of_typ (Type_check.typ_of exp)
  | Pat_when _ -> Ty_nys

(******************************************************************************)

let rec binds_of_pat (P_aux (aux, a)) =
  match aux with
  | P_lit lit ->
      let x = string_of_lit lit in
      let ty = ty_of_typ (Type_check.typ_of_annot a) in
      [(x, ty)]
  | P_id id ->
      let x = string_of_id id in
      let ty = ty_of_typ (Type_check.typ_of_annot a) in
      [(x, ty)]
  | P_tup pats ->
      List.concat (List.map binds_of_pat pats)
  | _ ->
      [] (* Not yet supported *)

let binds_of_pexp (Pat_aux (aux, _)) = 
  match aux with
  | Pat_exp (pat, _) -> binds_of_pat pat
  | Pat_when _ -> [] (* Not yet supported *)

(******************************************************************************)

let value_of_lit (L_aux (aux, _)) =
  match aux with
  | L_true  -> Val_bool true
  | L_false -> Val_bool false
  | L_num n -> Val_int n
  | _ -> Val_nys

let rec expression_of_aval = function
  | AV_lit (lit, _) ->
      Exp_val (value_of_lit lit)
  | AV_id (id, _) ->
      Exp_var (string_of_id id)
  | AV_tuple (h :: t) ->
      let e_h = expression_of_aval h in
      let f e1 aval2 =
        let e2 = expression_of_aval aval2 in
        Exp_binop (Pair, e1, e2) in
      List.fold_left f e_h t
  | AV_list (l, _) ->
      Exp_list (List.map expression_of_aval l)
  | _ -> Exp_nys

let rec statement_of_aexp (AE_aux (aux, _, _)) =
  match aux with
  | AE_val aval ->
      Stm_exp (expression_of_aval aval)
  | AE_app (id, avals, _) -> 
      let x = string_of_id id in
      (match avals with
      | [aval1; aval2] when x = "sail_cons" ->
          let e1 = expression_of_aval aval1 in
          let e2 = expression_of_aval aval2 in
          Stm_exp (Exp_binop (Cons, e1, e2))
      | _ ->
          Stm_call (x, List.map expression_of_aval avals))
  | AE_let (_, id, _, aexp1, aexp2, _) ->
      let x = string_of_id id in
      let s1 = statement_of_aexp aexp1 in
      let s2 = statement_of_aexp aexp2 in
      Stm_let (x, s1, s2)
  | AE_case (aval, cases, _) ->
      statement_of_match aval cases
  | _ -> Stm_nys

and statement_of_match aval cases =
  let simple_list_cases_opt = function
    | [(AP_aux (AP_nil _, _, _), _, aexp1); (AP_aux (AP_cons (
          AP_aux ((AP_id (id_h, _)), _, _),
          AP_aux ((AP_id (id_t, _)), _, _)
        ), _, _), _, exp2)] -> Some (aexp1, id_h, id_t, exp2)
    | _ -> None
  in match simple_list_cases_opt cases with
  | Some (aexp1, id_h, id_t, aexp2) ->
      Stm_match_list {
        s        =  Stm_exp (expression_of_aval aval);
        alt_nil  = statement_of_aexp aexp1;
        xh       = string_of_id id_h;
        xt       = string_of_id id_t;
        alt_cons = statement_of_aexp aexp2;
      }
  | _ -> Stm_nys

let body_of_pexp (Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (_, exp) -> statement_of_aexp (anf exp)
  | Pat_when _       -> failwith "`when` not yet processed"

(******************************************************************************)

let ir_funcl (FCL_aux (FCL_Funcl (id, pexp), _)) = {
  name    = string_of_id(id);
  funType = {
    arg_types = binds_of_pexp pexp;
    ret_type  = ty_of_pexp pexp
  };
  funBody = body_of_pexp pexp
}

let ir_fundef (FD_aux ((FD_function (_, _, funcls)), _)) =
  match funcls with
  | [funcl] -> some (ir_funcl funcl)
  | _       -> none


let ir_def = function
  | DEF_fundef fd -> join (some (ir_fundef fd))
  | _             -> none 

let ast_to_ir {defs; _} name =  {
  program_name = name;
  funDefList   = List.filter_map ir_def defs
}
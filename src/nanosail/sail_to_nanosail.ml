open Option
open Ast
open Libsail.Ast
open Libsail.Ast_defs
open Libsail.Anf
open Libsail

module Big_int = Nat_big_num


(******************************************************************************)

let string_of_id (Id_aux (aux, _)) =
  match aux with
  | Id x -> x
  | _ -> " NOT YET SUPPORTED "


(******************************************************************************)

let ty_id_of_typ_id (Id_aux (aux, _)) =
  match aux with
  | Id "bool"   -> Bool
  | Id "int"    -> Int
  | Id "list"   -> List
  | Id "prod"   -> Prod
  | Id "unit"   -> Unit
  | Id "string" -> String
  | _ -> Id_nys

let rec ty_of_typ (Typ_aux (typ, _)) =
  let ty_of_arg (A_aux (aux, _)) = 
    match aux with
    | A_typ typ -> ty_of_typ typ
    | _ -> Ty_nys in
  match typ with
  | Typ_id id          -> Ty_id (ty_id_of_typ_id id)
  | Typ_app (id, args) -> (
      match string_of_id id with
      | "atom"      -> Ty_id Int
      | "atom_bool" -> Ty_id Bool
      | _           -> Ty_app (ty_id_of_typ_id id, List.map ty_of_arg args)
    )
  | Typ_tup (h_typ :: typs) ->
      let h_ty = ty_of_typ h_typ in
      let f ty1 typ2 =
        let ty2 = ty_of_typ typ2 in
        Ty_app (Prod, [ty1; ty2]) in
      List.fold_left f h_ty typs
  | _ -> Ty_nys


(******************************************************************************)

let ty_of_pexp (Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (_, exp) -> ty_of_typ (Type_check.typ_of exp)
  | Pat_when _ -> Ty_nys


(******************************************************************************)

let rec binds_of_pat (P_aux (aux, a)) =
  match aux with
  | P_lit (L_aux (L_unit, _)) -> [("()", Ty_id Unit)]
  | P_id id ->
      let x = string_of_id id in
      let ty = ty_of_typ (Type_check.typ_of_annot a) in
      [(x, ty)]
  | P_tup pats ->
      List.concat (List.map binds_of_pat pats)
  | _ ->
      [("PATTERN_NOT_YET_SUPPORTED", Ty_nys)]

let binds_of_pexp (Pat_aux (aux, _)) = 
  match aux with
  | Pat_exp (pat, _) -> binds_of_pat pat
  | Pat_when _ ->
      [("PATTERN_NOT_YET_SUPPORTED", Ty_nys)]


(******************************************************************************)

let value_of_lit (L_aux (aux, _)) =
  match aux with
  | L_true     -> Val_bool true
  | L_false    -> Val_bool false
  | L_num n    -> Val_int n
  | L_unit     -> Val_unit
  | L_string s -> Val_string s
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
      let x = string_of_id id in (
        match avals with
        | [aval1; aval2] when x = "sail_cons" ->
            let e1 = expression_of_aval aval1 in
            let e2 = expression_of_aval aval2 in
            Stm_exp (Exp_binop (Cons, e1, e2))
        | _ ->
            Stm_call (x, List.map expression_of_aval avals)
      )
  | AE_let (_, id, _, aexp1, aexp2, _) ->
      let x = string_of_id id in
      let s1 = statement_of_aexp aexp1 in
      let s2 = statement_of_aexp aexp2 in
      Stm_let (x, s1, s2)
  | AE_if (aval, aexp1, aexp2, _) ->
      let s = Stm_exp (expression_of_aval aval) in
      let s1 = statement_of_aexp aexp1 in
      let s2 = statement_of_aexp aexp2 in
      Stm_if (s, s1, s2)
  | AE_case (aval, cases, _) ->
      statement_of_match aval cases
  | _ -> Stm_nys

and statement_of_match aval = function
  | [ (AP_aux (AP_nil _, _, _), _, aexp1);
      (AP_aux (AP_cons (
        AP_aux ((AP_id (id_h, _)), _, _),
        AP_aux ((AP_id (id_t, _)), _, _)
      ), _, _), _, aexp2)
    ] ->
      Stm_match_list {
        s        = Stm_exp (expression_of_aval aval);
        alt_nil  = statement_of_aexp aexp1;
        xh       = string_of_id id_h;
        xt       = string_of_id id_t;
        alt_cons = statement_of_aexp aexp2;
      }
  | [ (AP_aux (AP_cons (
        AP_aux ((AP_id (id_h, _)), _, _),
        AP_aux ((AP_id (id_t, _)), _, _)
      ), _, _), _, aexp1);
      (AP_aux (AP_nil _, _, _), _, aexp2)
    ] ->
      Stm_match_list {
        s        = Stm_exp (expression_of_aval aval);
        alt_nil  = statement_of_aexp aexp2;
        xh       = string_of_id id_h;
        xt       = string_of_id id_t;
        alt_cons = statement_of_aexp aexp1;
      }
  | [ (AP_aux (AP_tup [
        AP_aux ((AP_id (id_l, _)), _, _);
        AP_aux ((AP_id (id_r, _)), _, _);
      ], _, _),_ , aexp)
    ] -> 
      Stm_match_prod {
        s   = Stm_exp (expression_of_aval aval);
        xl  = string_of_id id_l;
        xr  = string_of_id id_r;
        rhs = statement_of_aexp aexp;
      }
  | _ -> Stm_nys

let body_of_pexp (Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (_, exp) -> statement_of_aexp (anf exp)
  | Pat_when _       -> Stm_nys


(******************************************************************************)

let ir_funcl (FCL_aux (FCL_Funcl (id, pexp), _)) = {
  funName = string_of_id(id);
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

let sail_to_nanosail {defs; _} name =  {
  program_name = name;
  funDefList   = List.filter_map ir_def defs
}
open Option
open Libsail.Ast
open Libsail.Ast_defs
open Libsail
open Nanosail.Ast

module Big_int = Nat_big_num

(******************************************************************************)

(*
module Fresh :
	sig
		val getFresh : unit -> string
		val reset : unit -> unit
	end = struct
		let var = ref 0
		let getFresh () = let n = !var in var := n + 1; "#" ^ (string_of_int n)
		let reset () = var := 0
	end
*)

(******************************************************************************)
(* Functions from Libsail.Ast_util *)

let string_of_id (Id_aux (aux, _)) =
  match aux with
  | Id x -> x
  | Operator x -> "(operator " ^ x ^ ")"

let string_of_lit (L_aux (lit, _)) =
  match lit with
  | L_unit -> "()"
  | L_zero -> "bitzero"
  | L_one -> "bitone"
  | L_true -> "true"
  | L_false -> "false"
  | L_num n -> Big_int.to_string n
  | L_hex n -> "0x" ^ n
  | L_bin n -> "0b" ^ n
  | L_undef -> "undefined"
  | L_real r -> r
  | L_string str -> "\"" ^ str ^ "\""

(******************************************************************************)

let ty_id_of_typ_id (Id_aux (aux, _)) =
  match aux with
  | Id "bool" -> Bool
  | Id "int"  -> Int
  | Id "list" -> List
  | Id "prod" -> Prod
  | Id "unit" -> Unit
  | _ -> Id_nyp

let rec ty_of_typ (Typ_aux (typ, _)) =
  let ty_of_arg (A_aux (aux, _)) = 
    match aux with
    | A_typ typ -> ty_of_typ typ
    | _         -> failwith "not yet processed" 
  in match typ with
  | Typ_id id -> Ty_id (ty_id_of_typ_id id)
  | Typ_app (id, args) -> Ty_app (ty_id_of_typ_id id, List.map ty_of_arg args)
  | Typ_tuple typs -> Ty_app (Prod, List.map ty_of_typ typs)
  | _ -> Ty_nyp

(******************************************************************************)

let ty_of_pexp (Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (_, exp) -> ty_of_typ (Type_check.typ_of exp)
  | Pat_when _       -> failwith "`when` not yet processed"

(******************************************************************************)

let rec binds_of_pat (P_aux (aux, a)) =
  match aux with
  | P_lit lit -> [(string_of_lit lit, ty_of_typ (Type_check.typ_of_annot a))]
  | P_id id -> [(string_of_id id, (ty_of_typ (Type_check.typ_of_annot a)) )]
  | P_tuple pats -> List.concat (List.map binds_of_pat pats)
  | _ -> []

let binds_of_pexp (Pat_aux (aux, _)) = 
  match aux with
  | Pat_exp (pat, _) -> binds_of_pat pat
  | Pat_when _       -> failwith "`when` not yet processed"

(******************************************************************************)

let value_of_lit (L_aux (aux, _)) =
  match aux with
  | L_true -> Val_bool true
  | L_false -> Val_bool false
  | L_num n -> Val_int n
  | _ -> Val_nyp

let rec expression_of_exp (E_aux (aux, _)) =
  match aux with
  | E_lit lit           -> Exp_val (value_of_lit lit)
  | E_list l            -> Exp_list (List.map expression_of_exp l)
  | E_id id             -> Exp_var (string_of_id id)
  | E_tuple (e :: es)   -> List.fold_left
      (fun a b -> Exp_binop (Pair, a, expression_of_exp b))
      (expression_of_exp e)
      es 
  | E_cons (hexp, texp) ->
      Exp_binop (Cons, expression_of_exp hexp, expression_of_exp texp)
  | _ -> Exp_nyp


(*
let has_list_typ exp =
  match Type_check.typ_of exp with
  | Typ_aux (Typ_app (Id_aux (Id "list", _), _), _) -> true
  | _ -> false
*)

let rec statement_of_exp ((E_aux (aux, _)) as e) =
  match aux with
  | E_match (exp, pexps) -> statement_of_match exp pexps
  | E_lit lit            -> Stm_val (value_of_lit lit)
  | E_app (id, exps)     -> Stm_call (string_of_id id, List.map
      expression_of_exp exps)
  | E_let (letbind, exp) -> statement_of_let letbind exp
  
  | E_list _             -> Stm_exp (expression_of_exp e)
  | E_id _               -> Stm_exp (expression_of_exp e)
  | E_cons _             -> Stm_exp (expression_of_exp e)
  | E_tuple _            -> Stm_exp (expression_of_exp e)

  | _ -> Stm_nyp

and statement_of_let (LB_aux (LB_val (P_aux (aux, _), exp1), _)) exp2 =
  match aux with
  | P_id id -> Stm_let (string_of_id id, statement_of_exp exp1,
      statement_of_exp exp2)
  | _ -> failwith "not a single id binding"

and statement_of_match exp pexps =
  match match_list_opt pexps with
  | Some (exp1, id_h, id_t, exp2) ->
      Stm_match_list {
        s        = statement_of_exp exp;
        alt_nil  = statement_of_exp exp1;
        xh       = string_of_id id_h;
        xt       = string_of_id id_t;
        alt_cons = statement_of_exp exp2;
      }
  | _ -> Stm_nyp

and match_list_opt = function
  | [Pat_aux (Pat_exp (P_aux (P_list [], _), exp1), _); Pat_aux (Pat_exp
        (P_aux (P_cons (P_aux (P_id id_h, _), P_aux (P_id id_t, _)), _),
        exp2), _)] ->
      Some (exp1, id_h, id_t, exp2)
  | _ -> None

let body_of_pexp (Pat_aux (aux, _)) =
  match aux with
  | Pat_exp (_, exp) -> statement_of_exp exp
  | Pat_when _       -> failwith "`when` not yet processed"

(******************************************************************************)

let ir_funcl (FCL_aux (FCL_funcl (id, pexp), _)) = {
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


let ir_def (DEF_aux (aux, _)) =
  match aux with
  | DEF_fundef fd -> join (some (ir_fundef fd))
  | _             -> none 

let ast_to_ir {defs; _} name =  {
  program_name = name;
  funDefList   = List.filter_map ir_def defs
}
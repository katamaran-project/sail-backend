




(******************************************************************************)

type ty =
  | Int
  | List of ty
  | Bool
  | Unit
  | String
  | Prod of ty * ty
  (*
  | Sum of ty * ty
  | Undecide
  *)

type bind = string * ty


type funType_t = {
  arg_types : bind list;
  ret_type  : ty
}

(******************************************************************************)

type value =
  | Val_bool of bool
  | Val_int of int
  (*
  | Val_list of value list 
  *)
  | Val_unit
  | Val_string of string
  | Val_prod of value * value
  (*
  | Val_Sum of value + value
  *)

let rec ty_val = function
  | Val_bool _        -> Bool
  | Val_int _         -> Int
  | Val_unit          -> Unit
  (*
  | Val_list []       -> List Undecide
  | Val_list (h :: _) -> List (ty_val h)
  *)
  | Val_string _      -> String
  | Val_prod (v1, v2) -> Prod (ty_val v1, ty_val v2)
  (*
  | Val_Sum v         -> Sum (ty_val v, Undecide 
  *)

type expression =
  | Exp_var of string
  | Exp_val of value
  | Exp_list of expression list

type statement =
  | Stm_val of value
  | Stm_exp of expression
  | Stm_match_list of {
      s        : statement; 
      alt_nil  : statement;
      xh       : string;
      xt       : string;
      alt_cons : statement
    }
    (*
  | Stm_call of string * ??? 
    *)


(******************************************************************************)

type funDef_t = {
  name    : string;
  funType : funType_t;
  funBody : statement
}

(******************************************************************************)

type ir_t = {
  program_name : string;
  funDefList : funDef_t list
}


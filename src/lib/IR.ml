




(******************************************************************************)

type ty =
  | Int
  | List of ty
  | Bool
  | Unit
  | Undecide
  (*
  | String
  | Prod of ty * ty
  | Sum of ty * ty
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
  | Val_list of value list 
  | Val_unit

let rec ty_val = function
  | Val_bool _        -> Bool
  | Val_int _         -> Int
  | Val_unit          -> Unit
  | Val_list []       -> List Undecide
  | Val_list (h :: _) -> List (ty_val h)

type expression =
  | Exp_var of string
  | Exp_val of value

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







(******************************************************************************)

type ty =
  | Unit
  | Bool
  | Int
  | String
  | List of ty
  | Prod of ty * ty

type bind = string * ty


type funType_t = {
  arg_types : bind list;
  ret_type  : ty
}

(******************************************************************************)

type binOp =
  | Plus
  | Times
  | Minus
  | And
  | Or
  | Pair
  | Cons
  | Append
  | Eq
  | Neq
  | Le 
  | Lt
  | Ge
  | Gt


(******************************************************************************)

type value =
  | Val_unit
  | Val_bool of bool
  | Val_int of int
  | Val_string of string
  | Val_prod of value * value

let rec ty_val = function
  | Val_unit          -> Unit
  | Val_bool _        -> Bool
  | Val_int _         -> Int
  | Val_string _      -> String
  | Val_prod (v1, v2) -> Prod (ty_val v1, ty_val v2)

(******************************************************************************)

type expression =
  | Exp_var of string
  | Exp_val of value
  | Exp_neg of expression
  | Exp_not of expression
  | Exp_list of expression list
  | Exp_binop of binOp * expression * expression

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
  | Stm_call of string * expression list 
  | Stm_let of string * statement * statement


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


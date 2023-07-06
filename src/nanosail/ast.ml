

module Big_int = Nat_big_num

(******************************************************************************)

type ty_id =
  | Unit
  | Bool
  | Int
  | String
  | List
  | Prod
  | Id_nyp

type ty =
  | Ty_id of ty_id
  | Ty_app of ty_id * ty list
  | Ty_nyp

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
  | Val_int of Big_int.num
  | Val_string of string
  | Val_prod of value * value
  | Val_nyp

let rec ty_val = function
  | Val_unit          -> Ty_id Unit
  | Val_bool _        -> Ty_id Bool
  | Val_int _         -> Ty_id Int
  | Val_string _      -> Ty_id String
  | Val_prod (v1, v2) -> Ty_app (Prod, [ty_val v1; ty_val v2])
  | Val_nyp           -> Ty_nyp

(******************************************************************************)

type expression =
  | Exp_var of string
  | Exp_val of value
  | Exp_neg of expression
  | Exp_not of expression
  | Exp_list of expression list
  | Exp_binop of binOp * expression * expression
  | Exp_nyp

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
  | Stm_nyp


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


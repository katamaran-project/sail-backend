
(******************************************************************************)




(******************************************************************************)

type ty =
  | Int
  | List of ty
  | Bool
  (*
  | String
  | Prod of ty * ty
  | Sum of ty * ty
  | Unit
  *)

type bind = string * ty


type funType_t = {
  arg_types : bind list;
  ret_type  : ty
}

(******************************************************************************)

(*
type 'a value =
  | Val_bool of bool constraint 'a = bool
  | Val_int of int constraint 'a = int 
  | Val_list of 'b constraint 'a = 'b list
*)

type value =
  | Val_bool of bool
  | Val_int of int
  | Val_list of value list 

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


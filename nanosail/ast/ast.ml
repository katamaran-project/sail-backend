(** NanoSail AST definition *)

module Big_int = Nat_big_num


type sail_definition = Libsail.Type_check.tannot Libsail.Ast.def

(******************************************************************************)
(* Numeric expressions *)


type numeric_expression =
  | NE_constant of Z.t
  | NE_add of numeric_expression * numeric_expression
  | NE_minus of numeric_expression * numeric_expression
  | NE_times of numeric_expression * numeric_expression
  | NE_neg of numeric_expression
  | NE_id of string
  | NE_var of string



(******************************************************************************)
(* Built-in types *)

type ty_id =
  | Unit
  | Bool
  | Int
  | String
  | List
  | Prod
  | Bitvector
  | Atom
  | Id_nys                                 (* For typ ids not yet supported      *)

type ty =
  | Ty_id of ty_id                         (* For concrete types                 *)
  | Ty_app of ty_id * type_argument list   (* For type constructors              *)
  | Ty_nys                                 (* For typ variants not yet supported *)
and type_argument =
  | TA_type of ty
  | TA_numexp of numeric_expression

type bind = string * ty

type funType_t = {
  arg_types : bind list;
  ret_type  : ty
}




(******************************************************************************)
(* Binary operators *)

type binOp =
  (* Only Cons and Pair are generated by the sail to nanosail translator at the 
     moment *)
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
(* Values *)

type value =
  | Val_unit
  | Val_bool of bool
  | Val_int of Big_int.num
  | Val_string of string
  | Val_prod of value * value
  | Val_nys   (* For value types not yet supported *)


(******************************************************************************)
(* Statements and expressions *)

type expression =
  | Exp_var of string
  | Exp_val of value
  | Exp_neg of expression     (* Not yet used by the sail to nanosail translator *)
  | Exp_not of expression     (* Not yet used by the sail to nanosail translator *)
  | Exp_list of expression list
  | Exp_binop of binOp * expression * expression
  | Exp_nys                   (* For expression types not yet supported *)

type statement =
  | Stm_exp of expression
  | Stm_match_list of {       (* Simple pattern matching on list (2 cases) *)
      s        : statement;   (* Statement to match *)
      alt_nil  : statement;   (* Nil case statement *)
      xh       : string;      (* Variable name for the head of the list *)
      xt       : string;      (* Variable name for the tail of the list *)
      alt_cons : statement;   (* Cons case statement *)
    } 
  | Stm_match_prod of {       (* Simple pattern matching on product (1 case) *)
      s   : statement;        (* Statement to match *)
      xl  : string;           (* Variable name for the left term of the product *)
      xr  : string;           (* Variable name for the right tern of the product *)
      rhs : statement;        (* Resulting statement *)
    }
  | Stm_call of string * expression list    (* AST already in A-normal form *)
  | Stm_let of string * statement * statement
  | Stm_if of statement * statement * statement
  | Stm_nys                                 (* For statement types not yet supported *)


(******************************************************************************)
(* Function definitions *)

type function_definition = {
  funName : string;
  funType : funType_t;
  funBody : statement;
}

(******************************************************************************)
(* Type definitions *)

type type_abbreviation =
  | TA_numeric_expression of numeric_expression

type type_definition =
  | TD_abbreviation of (string * type_abbreviation)

(******************************************************************************)
(* Definitions *)

type untranslated_definition =
  {
    filename: string;
    line_number: int;
    sail_location: Libsail.Parse_ast.l;
    message: string option
  }

type register_definition =
  {
    identifier: string;
    typ: ty
  }

type enum_definition =
  {
    identifier: string;
    cases: string list
  }

type definition =
  | FunctionDefinition of function_definition
  | TypeDefinition of type_definition
  | RegisterDefinition of register_definition
  | EnumDefinition of enum_definition
  | UntranslatedDefinition of untranslated_definition
  | IgnoredDefinition

let extract_function_definition = function
  | FunctionDefinition x -> Some x
  | _                    -> None

let extract_type_definition = function
  | TypeDefinition x -> Some x
  | _                -> None

let extract_enum_definition = function
  | EnumDefinition x -> Some x
  | _                -> None

let extract_register_definition = function
  | RegisterDefinition x -> Some x
  | _                    -> None

let extract_untranslated_definition = function
  | UntranslatedDefinition x -> Some x
  | _                        -> None

let extract_ignored_definition = function
  | IgnoredDefinition -> Some ()
  | _                 -> None

(******************************************************************************)
(* Full intermediate representation *)

(** The type of the NanoSail intermediate representation. *)
type ir_t = {
  program_name : string;
  function_definitions : (sail_definition * function_definition) list;
  type_definitions: (sail_definition * type_definition) list;
  enum_definitions: (sail_definition * enum_definition) list;
  register_definitions: (sail_definition * register_definition) list;
  untranslated_definitions : (sail_definition * untranslated_definition) list;
  ignored_definitions : sail_definition list
  (* Other record fields will need to be added to extend the language (e.g. one
     for user types and one for registers). *)
}

let make_ir_t
      ?(function_definitions : (sail_definition * function_definition) list = [])
      ?(type_definitions : (sail_definition * type_definition) list = [])
      ?(enum_definitions : (sail_definition * enum_definition) list = [])
      ?(register_definitions : (sail_definition * register_definition) list = [])
      ?(untranslated_definitions : (sail_definition * untranslated_definition) list = [])
      ?(ignored_definitions : sail_definition list = [])
      program_name =
  {
    program_name = program_name;
    function_definitions = function_definitions;
    type_definitions = type_definitions;
    enum_definitions = enum_definitions;
    register_definitions = register_definitions;
    untranslated_definitions = untranslated_definitions;
    ignored_definitions = ignored_definitions;
  }

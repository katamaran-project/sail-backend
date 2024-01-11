open Base

(** NanoSail AST definition *)

module Big_int = Nat_big_num


type sail_definition = Libsail.Type_check.tannot Libsail.Ast.def

type identifier = string

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

type kind =
  | Kind_type
  | Kind_int
  | Kind_bool

type nanotype =
  | Ty_unit
  | Ty_bool
  | Ty_int
  | Ty_nat
  | Ty_string
  | Ty_atom
  | Ty_list      of nanotype
  | Ty_bitvector of numeric_expression
  | Ty_tuple     of nanotype list
  | Ty_app       of string * type_argument list
  | Ty_custom    of string

and type_argument =
  | TA_type   of nanotype
  | TA_numexp of numeric_expression
  | TA_bool   of numeric_constraint

and numeric_constraint =
  | NC_equal      of numeric_expression * numeric_expression
  | NC_bounded_ge of numeric_expression * numeric_expression
  | NC_bounded_gt of numeric_expression * numeric_expression
  | NC_bounded_le of numeric_expression * numeric_expression
  | NC_bounded_lt of numeric_expression * numeric_expression
  | NC_not_equal  of numeric_expression * numeric_expression
  | NC_set        of string             * Z.t list
  | NC_or         of numeric_constraint * numeric_constraint
  | NC_and        of numeric_constraint * numeric_constraint
  | NC_app        of string             * type_argument list
  | NC_var        of string
  | NC_true
  | NC_false

type type_quantifier_item = (string * kind)

type type_quantifier = type_quantifier_item list

type bind = string * nanotype

type funType_t = {
  arg_types : bind list;
  ret_type  : nanotype
}


(*
  If given type is a tuple, collects all types inside of it in a list.
  If given type is not a tuple, simply return that type in a singleton list.
 *)
let tuple_to_list (t : nanotype) : nanotype list =
  match t with
  | Ty_tuple ts -> ts
  | _           -> [ t ]


(******************************************************************************)
(* Binary operators *)

type binary_operator =
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


(******************************************************************************)
(* Statements and expressions *)

type expression =
  | Exp_var of string
  | Exp_val of value
  | Exp_neg of expression     (* Not yet used by the sail to nanosail translator *)
  | Exp_not of expression     (* Not yet used by the sail to nanosail translator *)
  | Exp_list of expression list
  | Exp_binop of binary_operator * expression * expression

type statement =
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
  | Stm_match of match_case list
  | Stm_exp   of expression
  | Stm_call  of string * expression list    (* AST already in A-normal form *)
  | Stm_let   of string * statement * statement
  | Stm_if    of statement * statement * statement
  | Stm_seq   of statement * statement
  | Stm_nys                                 (* For statement types not yet supported *)

and match_case =
  | MatchCase of pattern * statement

and pattern =
  | Pat_id of string

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
  | TA_numeric_expression of type_quantifier * numeric_expression
  | TA_numeric_constraint of type_quantifier * numeric_constraint
  | TA_alias              of type_quantifier * nanotype

type type_definition =
  | TD_abbreviation of (string * type_abbreviation)

type untranslated_definition =
  {
    filename      : string             ;
    line_number   : int                ;
    sail_location : Libsail.Parse_ast.l;
    message       : string option      ;
  }

type register_definition =
  {
    identifier : string  ;
    typ        : nanotype;
  }

type variant_definition =
  {
    identifier      : string                  ;
    type_quantifier : type_quantifier         ;
    constructors    : (string * nanotype) list;
  }

type enum_definition =
  {
    identifier : string     ;
    cases      : string list;
  }

type top_level_type_constraint_definition =
  {
    identifier : string;
  }

type definition =
  | TopLevelTypeConstraintDefinition of top_level_type_constraint_definition
  | FunctionDefinition               of function_definition
  | TypeDefinition                   of type_definition
  | RegisterDefinition               of register_definition
  | VariantDefinition                of variant_definition
  | EnumDefinition                   of enum_definition
  | UntranslatedDefinition           of untranslated_definition
  | IgnoredDefinition

type program = {
    program_name : string;
    definitions  : (sail_definition * definition) list   (* All translated definitions; original order preserved *)
  }


module Extract = struct
  let function_definition = function
    | FunctionDefinition x -> Some x
    | _                    -> None

  let type_definition = function
    | TypeDefinition x -> Some x
    | _                -> None

  let enum_definition = function
    | EnumDefinition x -> Some x
    | _                -> None

  let variant_definition = function
    | VariantDefinition x -> Some x
    | _                   -> None

  let register_definition = function
    | RegisterDefinition x -> Some x
    | _                    -> None

  let untranslated_definition = function
    | UntranslatedDefinition x -> Some x
    | _                        -> None

  let ignored_definition = function
    | IgnoredDefinition -> Some ()
    | _                 -> None

  let top_level_type_constraint_definition = function
    | TopLevelTypeConstraintDefinition x -> Some x
    | _                                  -> None
end

  
let select (extractor : definition -> 'a option) (definitions : (sail_definition * definition) list) =
  let lift_extractor extractor (sail_definition, definition) =
    Option.map ~f:(fun def -> (sail_definition, def)) (extractor definition)
  in
  List.filter_map ~f:(lift_extractor extractor) definitions

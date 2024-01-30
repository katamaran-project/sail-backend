open Base

module Big_int = Nat_big_num


type type_annotation = Libsail.Type_check.tannot

type sail_definition = type_annotation Libsail.Ast.def

type identifier = string

type numeric_expression =
  | NE_constant of Z.t
  | NE_add of numeric_expression * numeric_expression
  | NE_minus of numeric_expression * numeric_expression
  | NE_times of numeric_expression * numeric_expression
  | NE_neg of numeric_expression
  | NE_id of string
  | NE_var of string

type kind =
  | Kind_type
  | Kind_int
  | Kind_bool

(*
  should mirror

    Inductive Ty : Set :=
    | int
    | bool
    | string
    | list (σ : Ty)
    | prod (σ τ : Ty)
    | sum  (σ τ : Ty)
    | unit
    | enum (E : enumi)
    | bvec (n : nat)
    | tuple (σs : Ctx Ty)
    | union (U : unioni)
    | record (R : recordi)
    .

   defined in theories/Syntax/TypeDecl.v
 *)
type nanotype =
  | Ty_int
  | Ty_bool
  | Ty_string
  | Ty_list      of nanotype
  | Ty_prod      of nanotype * nanotype
  | Ty_sum       of nanotype * nanotype
  | Ty_unit
  (* | Ty_enum *)                                (* TODO add *)
  | Ty_bitvector of numeric_expression
  | Ty_tuple     of nanotype list
  (* | Ty_union *)                               (* TODO add *)
  | Ty_record                                    (* TODO complete *)

  | Ty_nat                                       (* TODO remove *)
  | Ty_atom                                      (* TODO remove *)
  | Ty_app       of string * type_argument list  (* TODO remove *)
  | Ty_custom    of string                       (* TODO remove *)

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

type function_type = {
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


type binary_operator =
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


type value =
  | Val_unit
  | Val_bool of bool
  | Val_int of Big_int.num
  | Val_string of string
  | Val_prod of value * value


type expression =
  | Exp_var of string
  | Exp_val of value
  | Exp_neg of expression     (* Not yet used by the sail to nanosail translator *)
  | Exp_not of expression     (* Not yet used by the sail to nanosail translator *)
  | Exp_list of expression list
  | Exp_binop of binary_operator * expression * expression


type statement =
  | Stm_match of match_pattern
  | Stm_exp   of expression
  | Stm_call  of string * expression list    (* AST already in A-normal form *)
  | Stm_let   of string * statement * statement
  | Stm_seq   of statement * statement

and match_pattern =
  | MP_list    of match_pattern_list
  | MP_product of match_pattern_product
  | MP_bool    of match_pattern_bool
  | MP_enum    of match_pattern_enum

and match_pattern_list =
  {
    matched   : statement;
    when_cons : string * string * statement;
    when_nil  : statement;
  }

and match_pattern_product =
  {
    matched   : statement;
    id_fst    : string;
    id_snd    : string;
    body      : statement;
  }

and match_pattern_bool =
  {
    condition  : statement;
    when_true  : statement;
    when_false : statement;
  }

and match_pattern_enum =
  {
    matched    : statement;
    cases      : statement StringMap.t
  }


type function_definition = {
  function_name : string;
  function_type : function_type;
  function_body : statement;
}


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


type type_abbreviation =
  | TA_numeric_expression of type_quantifier * numeric_expression
  | TA_numeric_constraint of type_quantifier * numeric_constraint
  | TA_alias              of type_quantifier * nanotype


type type_abbreviation_definition =
  {
    identifier   : string;
    abbreviation : type_abbreviation
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


type type_definition =
  | TD_abbreviation of type_abbreviation_definition
  | TD_variant      of variant_definition
  | TD_enum         of enum_definition


type value_definition =
  {
    identifier : string   ;
    value      : statement;
  }


type record_definition =
  {
    identifier      : string;
    type_quantifier : type_quantifier;
    fields          : (string * nanotype) list;
  }

type definition =
  | TopLevelTypeConstraintDefinition of top_level_type_constraint_definition
  | FunctionDefinition               of function_definition
  | TypeDefinition                   of type_definition
  | RegisterDefinition               of register_definition
  | UntranslatedDefinition           of untranslated_definition
  | ValueDefinition                  of value_definition
  | RecordDefinition                 of record_definition
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
    | TypeDefinition (TD_enum x) -> Some x
    | _                          -> None

  let variant_definition = function
    | TypeDefinition (TD_variant x) -> Some x
    | _                             -> None

  let abbreviation_definition = function
    | TypeDefinition (TD_abbreviation x) -> Some x
    | _                                  -> None

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

  let value_definition = function
    | ValueDefinition x -> Some x
    | _                 -> None
end


let select (extractor : definition -> 'a option) (definitions : (sail_definition * definition) list) =
  let lift_extractor extractor (sail_definition, definition) =
    Option.map ~f:(fun def -> (sail_definition, def)) (extractor definition)
  in
  List.filter_map ~f:(lift_extractor extractor) definitions


let type_identifier (type_definition : type_definition) =
  match type_definition with
  | TD_abbreviation x -> x.identifier
  | TD_variant x      -> x.identifier
  | TD_enum x         -> x.identifier

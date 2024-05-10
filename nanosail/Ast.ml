open Base

module Big_int = Nat_big_num


(* todo find better location for this definition *)
type type_annotation = Libsail.Type_check.tannot

(* todo find better location for this definition *)
type sail_definition = type_annotation Libsail.Ast.def

type identifier = Id.t

type numeric_expression =
  | NE_constant of Z.t
  | NE_add      of numeric_expression * numeric_expression
  | NE_minus    of numeric_expression * numeric_expression
  | NE_times    of numeric_expression * numeric_expression
  | NE_neg      of numeric_expression
  | NE_id       of identifier
  | NE_var      of identifier

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
  | Ty_app       of nanotype * type_argument list  (* TODO remove *)
  | Ty_custom    of identifier                       (* TODO remove *)

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
  | NC_set        of identifier         * Z.t list
  | NC_or         of numeric_constraint * numeric_constraint
  | NC_and        of numeric_constraint * numeric_constraint
  | NC_app        of identifier         * type_argument list
  | NC_var        of identifier
  | NC_true
  | NC_false

type type_quantifier_item = identifier * kind

type type_quantifier = type_quantifier_item list

type bind = identifier * nanotype

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
  | Val_bool   of bool
  | Val_int    of Big_int.num
  | Val_string of string
  | Val_prod   of value * value


type expression =
  | Exp_var    of identifier
  | Exp_val    of value
  | Exp_neg    of expression
  | Exp_not    of expression
  | Exp_list   of expression list
  | Exp_binop  of binary_operator * expression * expression
  | Exp_record of { type_identifier : identifier; variable_identifiers : identifier list }
  | Exp_enum   of identifier


type statement =
  | Stm_match              of match_pattern
  | Stm_exp                of expression
  | Stm_call               of identifier * expression list
  | Stm_let                of identifier * statement * statement
  | Stm_destructure_record of destructure_record
  | Stm_seq                of statement * statement
  | Stm_read_register      of identifier
  | Stm_write_register     of identifier * statement
  | Stm_cast               of statement * nanotype
  | Stm_fail               of string

and match_pattern =
  | MP_list    of match_pattern_list
  | MP_product of match_pattern_product
  | MP_bool    of match_pattern_bool
  | MP_enum    of match_pattern_enum
  | MP_variant of match_pattern_variant

and match_pattern_list =
  {
    matched   : statement;
    when_cons : identifier * identifier * statement;
    when_nil  : statement;
  }

and match_pattern_product =
  {
    matched   : statement;
    id_fst    : identifier;
    id_snd    : identifier;
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
    cases      : statement IdentifierMap.t
  }

and match_pattern_variant =
  {
    matched    : statement;
    cases      : (identifier list * statement) IdentifierMap.t
  }

and destructure_record =
  {
    record_type_identifier : identifier     ;   (* name of the record                                              *)
    field_identifiers      : identifier list;   (* names of the record's fields                                    *)
    variable_identifiers   : identifier list;   (* names of the variables receiving the record's fields' values    *)
    destructured_record    : statement      ;   (* statement yield the record object                               *)
    body                   : statement      ;   (* body that can refer to record fields using variable_identifiers *)
  }


type function_definition = {
  function_name : identifier;
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
    identifier : identifier;
    typ        : nanotype  ;
  }


type type_abbreviation =
  | TA_numeric_expression of type_quantifier * numeric_expression
  | TA_numeric_constraint of type_quantifier * numeric_constraint
  | TA_alias              of type_quantifier * nanotype


type type_abbreviation_definition =
  {
    identifier   : identifier       ;
    abbreviation : type_abbreviation;
  }


type variant_definition =
  {
    identifier      : identifier              ;
    type_quantifier : type_quantifier         ;
    constructors    : variant_constructor list;
  }

and variant_constructor = (identifier * nanotype list)


type enum_definition =
  {
    identifier : identifier;
    cases      : identifier list;
  }


type record_definition =
  {
    identifier      : identifier;
    type_quantifier : type_quantifier;
    fields          : (identifier * nanotype) list;
  }


type type_definition =
  | TD_abbreviation of type_abbreviation_definition
  | TD_variant      of variant_definition
  | TD_enum         of enum_definition
  | TD_record       of record_definition


type top_level_type_constraint_definition =
  {
    identifier : identifier;
  }


type value_definition =
  {
    identifier : identifier;
    value      : value;
  }


type definition =
  | TopLevelTypeConstraintDefinition of top_level_type_constraint_definition
  | FunctionDefinition               of function_definition
  | TypeDefinition                   of type_definition
  | RegisterDefinition               of register_definition
  | UntranslatedDefinition           of untranslated_definition
  | ValueDefinition                  of value_definition
  | IgnoredDefinition


type program = {
    program_name : string;
    definitions  : (sail_definition * definition) list   (* All translated definitions; original order preserved *)
  }


module Extract = struct
  let identity x = Some x


  let function_definition = function
    | FunctionDefinition x -> Some x
    | _                    -> None

  let type_definition (of_kind : type_definition -> 'a option) = function
    | TypeDefinition x -> of_kind x
    | _                -> None

  let of_anything = Option.some

  let of_enum = function
    | TD_enum x -> Some x
    | _         -> None

  let of_variant = function
    | TD_variant x -> Some x
    | _            -> None

  let of_record = function
    | TD_record x -> Some x
    | _           -> None

  let of_abbreviation = function
    | TD_abbreviation x -> Some x
    | _                 -> None

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
  | TD_record x       -> x.identifier

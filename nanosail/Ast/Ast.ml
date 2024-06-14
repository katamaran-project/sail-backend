module Big_int       = Nat_big_num

open Base


module Identifier           = Identifier
module NumericExpression    = NumericExpression
module Kind                 = Kind
module BinaryOperator       = BinaryOperator
module ExtendedType         = ExtendedType
module ExtendedFunctionType = ExtendedFunctionType
include Recursive


type type_quantifier_item = Identifier.t * Kind.t

type type_quantifier = type_quantifier_item list

type function_type = {
  parameters  : (Identifier.t * Type.t) list;
  return_type : Type.t
}


(*
  If given type is a tuple, collects all types inside of it in a list.
  If given type is not a tuple, simply return that type in a singleton list.
 *)
let tuple_to_list (t : Type.t) : Type.t list =
  match t with
  | Tuple ts -> ts
  | _        -> [ t ]


type value =
  | Val_unit
  | Val_bool   of bool
  | Val_int    of Big_int.num
  | Val_string of string
  | Val_prod   of value * value


type expression =
  | Exp_var    of Identifier.t
  | Exp_val    of value
  | Exp_neg    of expression
  | Exp_not    of expression
  | Exp_list   of expression list
  | Exp_binop  of BinaryOperator.t * expression * expression
  | Exp_record of { type_identifier : Identifier.t; variable_identifiers : Identifier.t list }
  | Exp_enum   of Identifier.t


type statement =
  | Stm_match              of match_pattern
  | Stm_exp                of expression
  | Stm_call               of Identifier.t * expression list
  | Stm_let                of Identifier.t * statement * statement
  | Stm_destructure_record of destructure_record
  | Stm_seq                of statement * statement
  | Stm_read_register      of Identifier.t
  | Stm_write_register     of Identifier.t * statement
  | Stm_cast               of statement * Type.t
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
    when_cons : Identifier.t * Identifier.t * statement;
    when_nil  : statement;
  }

and match_pattern_product =
  {
    matched   : statement;
    id_fst    : Identifier.t;
    id_snd    : Identifier.t;
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
    matched      : statement;
    matched_type : Identifier.t;
    cases        : statement Identifier.Map.t
  }

and match_pattern_variant =
  {
    matched    : statement;
    cases      : (Identifier.t list * statement) Identifier.Map.t
  }

and destructure_record =
  {
    record_type_identifier : Identifier.t     ;   (* name of the record                                              *)
    field_identifiers      : Identifier.t list;   (* names of the record's fields                                    *)
    variable_identifiers   : Identifier.t list;   (* names of the variables receiving the record's fields' values    *)
    destructured_record    : statement        ;   (* statement yield the record object                               *)
    body                   : statement        ;   (* body that can refer to record fields using variable_identifiers *)
  }


type function_definition = {
  function_name          : Identifier.t;
  function_type          : function_type;
  extended_function_type : ExtendedFunctionType.t;
  function_body          : statement;
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
    identifier : Identifier.t;
    typ        : Type.t      ;
  }


type type_abbreviation =
  | TA_numeric_expression of type_quantifier * NumericExpression.t
  | TA_numeric_constraint of type_quantifier * NumericConstraint.t
  | TA_alias              of type_quantifier * Type.t


type type_abbreviation_definition =
  {
    identifier   : Identifier.t     ;
    abbreviation : type_abbreviation;
  }


type variant_definition =
  {
    identifier      : Identifier.t            ;
    type_quantifier : type_quantifier         ;
    constructors    : variant_constructor list;
  }

and variant_constructor = (Identifier.t * Type.t list)


type enum_definition =
  {
    identifier : Identifier.t     ;
    cases      : Identifier.t list;
  }


type record_definition =
  {
    identifier      : Identifier.t                ;
    type_quantifier : type_quantifier             ;
    fields          : (Identifier.t * Type.t) list;
  }


type type_definition =
  | TD_abbreviation of type_abbreviation_definition
  | TD_variant      of variant_definition
  | TD_enum         of enum_definition
  | TD_record       of record_definition


type top_level_type_constraint_definition =
  {
    identifier : Identifier.t;
  }


type value_definition =
  {
    identifier : Identifier.t;
    value      : value       ;
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
    definitions  : (Sail.sail_definition * definition) list   (* All translated definitions; original order preserved *)
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


let select
    (extractor   : definition -> 'a option            )
    (definitions : (Sail.sail_definition * definition) list)
  =
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

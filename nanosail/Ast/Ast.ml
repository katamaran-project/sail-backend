open Base


module Identifier           = Identifier
module NumericExpression    = NumericExpression
module Kind                 = Kind
module BinaryOperator       = BinaryOperator
module ExtendedType         = ExtendedType
module ExtendedFunctionType = ExtendedFunctionType
module Value                = Value
include Recursive
module Statement            = Statement
module Expression           = Expression



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


type function_definition = {
  function_name          : Identifier.t;
  function_type          : function_type;
  extended_function_type : ExtendedFunctionType.t;
  function_body          : Statement.t;
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
    value      : Value.t     ;
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

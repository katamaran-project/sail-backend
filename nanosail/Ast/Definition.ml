include Recursive


type type_quantifier_item = Identifier.t * Kind.t


type type_quantifier = type_quantifier_item list


type function_type = {
  parameters  : (Identifier.t * Type.t) list;
  return_type : Type.t
}


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


type t =
  | TopLevelTypeConstraintDefinition of top_level_type_constraint_definition
  | FunctionDefinition               of function_definition
  | TypeDefinition                   of type_definition
  | RegisterDefinition               of register_definition
  | UntranslatedDefinition           of untranslated_definition
  | ValueDefinition                  of value_definition
  | IgnoredDefinition

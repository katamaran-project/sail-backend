include Recursive


type type_quantifier_item = Identifier.t * Kind.t


type type_quantifier = type_quantifier_item list


module FunctionType = struct
  type t = {
    parameters  : (Identifier.t * Type.t) list;
    return_type : Type.t
  }
end


module Function = struct
  type t = {
    function_name          : Identifier.t;
    function_type          : FunctionType.t;
    extended_function_type : ExtendedFunctionType.t;
    function_body          : Statement.t;
  }
end


module Untranslated = struct
  type t =
    {
      filename      : string             ;
      line_number   : int                ;
      sail_location : Libsail.Parse_ast.l;
      message       : string option      ;
    }
end


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


module Type = struct  
  module Variant = struct
    type t =
      {
        identifier      : Identifier.t    ;
        type_quantifier : type_quantifier ;
        constructors    : constructor list;
      }

    and constructor = (Identifier.t * Type.t list)
  end

  module Enum = struct
    type t =
      {
        identifier : Identifier.t     ;
        cases      : Identifier.t list;
      }
  end

  module Record = struct
    type t =
      {
        identifier      : Identifier.t                ;
        type_quantifier : type_quantifier             ;
        fields          : (Identifier.t * Type.t) list;
      }
  end


  type t =
    | TD_abbreviation of type_abbreviation_definition
    | Variant         of Variant.t
    | Enum            of Enum.t
    | Record          of Record.t
end
  

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
  | FunctionDefinition               of Function.t
  | TypeDefinition                   of Type.t
  | RegisterDefinition               of register_definition
  | UntranslatedDefinition           of Untranslated.t
  | ValueDefinition                  of value_definition
  | IgnoredDefinition

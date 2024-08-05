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


(* todo put in separate module *)
type register_definition =
  {
    identifier : Identifier.t;
    typ        : Type.t      ;
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

  module Abbreviation = struct
    type type_abbreviation = (* todo find better name *)
      | NumericExpression of type_quantifier * NumericExpression.t
      | NumericConstraint of type_quantifier * NumericConstraint.t
      | Alias             of type_quantifier * Type.t

    type t =
      {
        identifier   : Identifier.t     ;
        abbreviation : type_abbreviation;
      }
  end

  type t =
    | Abbreviation of Abbreviation.t
    | Variant      of Variant.t
    | Enum         of Enum.t
    | Record       of Record.t

  let identifier (type_definition : t) : Identifier.t =
    match type_definition with
    | Abbreviation x -> x.identifier
    | Variant x      -> x.identifier
    | Enum x         -> x.identifier
    | Record x       -> x.identifier
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

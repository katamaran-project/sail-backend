type enum_arguments =
  {
    type_identifier        : Identifier.t;
    constructor_identifier : Identifier.t
  }


type t =
  | Variable of Identifier.t
  | Val      of Value.t
  | Neg      of t
  | Not      of t
  | List     of t list
  | Binop    of BinaryOperator.t * t * t
  | Record   of { type_identifier : Identifier.t; variable_identifiers : Identifier.t list }
  | Enum     of enum_arguments
